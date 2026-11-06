program main
use mod_utility, only: lower_case, print_execution_time
use mod_date, only: date, annual_period_end, date_is_before, date_to_string
use mod_parameters, only: parameters, water_sources_table, source_info
use mod_grid, only: write_grid, min_domain, bound      ! variables and methods to handle spatial input
use mod_meteo
use mod_crop_phenology, only: crop_definition, crop_rotation, crop_pars_matrices
use mod_crop_yield, only: yield_accumulator
use mod_cropcoef, only: compute_kcb_corrections
use cli_crop_parameters, only: import_crop_definitions, initialize_co2_parameters
use cli_watsources                                                          ! variables and methods to handle irrigation units water supply
use cli_simulation_manager                                                  ! simulation manager: control IO and daily cycle
use cli_read_parameter
use mod_system
implicit none

type(parameters) :: xml                                     ! stores parameters of input file
type(bound)::boundaries                                     ! stores domain extension (rectangle)
type(spatial_info)::info_spat                               ! stores spatial inputs
type(water_sources_table),dimension(:),allocatable::watsour ! stores parameters of "<watsources>.txt" file
real(dp), dimension(10,3,4):: tab_CN2, tab_CN3              ! CN implementation
type(source_info)::info_sources                             ! stores water sources series
type(meteo_info),dimension(:),allocatable::info_meteo       ! stores meteorological series data
type(crop_definition), dimension(:), allocatable :: crops   ! shared static crop parameters
type(crop_definition) :: bare_soil                          ! parameters used for bare soil. If present, a 0_*.dat crop file overwrites default values.
type(crop_rotation), dimension(:), allocatable :: crop_rotations     ! crop IDs ordered by land use
real(dp), allocatable :: kcb_corr_fact(:,:)                 ! Additive kcb correction factor, indexed by crop ID and weather station.
type(crop_pars_matrices) :: crop_state                      ! per-cell state carried through warmup
type(yield_accumulator) :: crop_yield_state                 ! crop sums carried through warmup and calendar years
type(soil2_rice)::theta2_rice                               ! stores soil parameters data for paddy rice fields
type(date) :: warmup_end                                    ! last day of the full warmup period
real(dp), allocatable :: warmup_theta(:, :, :)              ! passes soil theta from warmup to the actual simulation

integer,dimension(8)::t_start,t_stop
character(len = 255) :: filename = 'idragra_parameters.txt'

character(len=50) :: arg
integer :: i
! default settings
logical :: verbose = .false.
logical :: summary = .false.
logical :: showpreview = .false.
logical :: reckless = .false.

! get options
do i = 1, iargc()
    call getarg(i, arg)
    if (arg(1:1)=='-') then
        call lower_case(arg)
        select case (arg)
            case ('-verbose', '-v')         ! set verbose mode: add some extra message to prompt
                verbose = .true.
            case ('-summary','-s')          ! set if only irrigation is printed
                summary = .true.
            case ('-help', '-h')            ! show help string
                call print_header()
                call show_help()
            case ('-filename', '-f')        ! set filename
                call getarg(i+1, arg)
                filename = arg
            case ('-preview', '-p')         ! show all parameters after reading configuration file
                showpreview = .true.
            case ('-default', '-d')         ! print all default value
                call print_header()
                print *, '=== DEFAULT VALUES ==='
                call print_parameters(xml)
                print *, '=== END DEFAULT ==='
                stop
            case ('-reckless', '-r')        ! doesn't stop for confirmation before overwriting output folder
                reckless = .true.
            case default                    ! all other cases ...
                call print_header()
                print *, 'Not supported option <',trim(arg),'>'
                call show_help()
        end select
    end if
end do

! TODO: check if there are other strings without '-', as it can be related to a skipped -f option

! set up environment options

call print_header()
! Memorization of time in which simulation starts
call date_and_time(values=t_start)

! Reads simulation input parameters
call read_all_parameters(filename, xml, verbose, reckless)

! Import crop .dat files and the list of landuses (rotations)
call import_crop_definitions(xml%sim, crops, crop_rotations, bare_soil)

if (showpreview .eqv. .true.) then
    print *, '=== PREVIEW ==='
    call print_parameters(xml)
    print *, '=== END PREVIEW ==='
end if
print*,'Simulation parameters have been set'

! Evaluates simulation length [days] by counting meteorological files' lines
call meteo_series_length(xml%sim, verbose)
call initialize_co2_parameters(xml%sim)

! Defines simulation domain boundaries
call min_domain(trim(xml%sim%input_path)//trim(xml%sim%domain_fn)//'.asc',boundaries,xml%sim)
print*,"Simulation domain boundaries have been set"

! Checks if soil initial condition is an input or it is generated by running first year of simulation
if(xml%sim%f_init_wc .eqv. .false.)then ! Generates soil initial condition

    ! Reuse one complete year of input data beginning on the simulation start date.
    warmup_end = annual_period_end(xml%sim%start)

    ! Initializes info_spat and reads spatial files (*.asc); initializes tab_CN*
    call read_spatial_info(info_spat,boundaries,xml%sim,tab_CN2,tab_CN3,theta2_rice,xml%irr%met)
    print*,"Variable info_spat has been initialized"
    if (xml%sim%prt_debug_out=='y') then
        ! Print spatial data matrices in files out_*.asc
        call write_init_grids(info_spat,xml%sim%mode,xml%sim%path,xml%sim)
        print*,"Input maps have been printed"
    end if

    print *, '=== INITIAL CONDITION ==='
    ! Initializes info_meteo matrices with meteorological data
    call read_meteo_parameters(xml%sim,info_meteo,verbose)
    print*, 'Variable "info_meteo" has been initialized'

    call compute_kcb_corrections(kcb_corr_fact, crops, info_meteo, xml%sim)

    if (any(date_is_before(info_meteo(:)%finish, warmup_end))) then
        print *, 'A complete warmup requires meteorological data from '//date_to_string(xml%sim%start)// &
               & ' through '//date_to_string(warmup_end)//'.'
        print *, 'The available meteorological series ends on '//date_to_string(info_meteo(1)%finish)//'.'
        print *, 'Execution will be aborted...'
        stop
    end if

    ! Initializes watsources and info_sources matrices
    if (xml%sim%mode == 1) then                 ! USE mode
        call init_water_sources_duty(xml,watsour,info_sources,info_meteo)
        print*, 'Variables "watsour" and "info_sources" have been initialized'
    end if

    ! Soil-crop water balance algorithm
    call simulation_manager(xml, info_spat, watsour, info_sources, info_meteo, crops, bare_soil,           &
                          & crop_rotations, kcb_corr_fact, crop_state, crop_yield_state, tab_CN2, tab_CN3, &
                          & theta2_rice, warmup_end, boundaries, verbose, summary, is_warmup=.true.        )

    where (crop_state%sowing_year > 0) crop_state%sowing_year = crop_state%sowing_year - 1
    where (crop_yield_state%sowing_year > 0) crop_yield_state%sowing_year = crop_yield_state%sowing_year - 1

    !%PS%: save soil moisture at the end of warmup explicitly (info_spat gets overwritten by read_spatial_info)
    allocate(warmup_theta(size(info_spat%theta(1)%old%mat, 1), size(info_spat%theta(1)%old%mat, 2), 2))
    warmup_theta(:, :, 1) = info_spat%theta(1)%old%mat
    warmup_theta(:, :, 2) = info_spat%theta(2)%old%mat

    ! Prints initial condition values
    if (xml%sim%prt_init_cond == 'y') then
        call write_grid(trim(xml%sim%path)//'IC_thetaI.asc',info_spat%theta(1)%old)
        call write_grid(trim(xml%sim%path)//'IC_thetaII.asc',info_spat%theta(2)%old)
    end if

    ! Closes input files
    if (xml%sim%mode == 1) call close_water_sources_dudy(info_sources,xml)        ! USE mode
    call close_meteo_file(info_meteo)
    print *, '=== INITIAL CONDITION SET ==='
end if

! Initializes info_spat and reads spatial files (*.asc); initializes tab_CN*
call read_spatial_info(info_spat,boundaries,xml%sim,tab_CN2,tab_CN3,theta2_rice,xml%irr%met)

!%PS%: immediately overwrite starting theta set in read_spatial_info with warmup_theta if warmup was performed
if (allocated(warmup_theta)) then
    info_spat%theta(1)%old%mat = warmup_theta(:, :, 1)
    info_spat%theta(2)%old%mat = warmup_theta(:, :, 2)
    deallocate(warmup_theta)
end if

print*,"Variable info_spat has been initialized"
if (xml%sim%prt_debug_out=='y') then
    ! Print spatial data matrices in files out_*.asc
    call write_init_grids(info_spat,xml%sim%mode,xml%sim%path,xml%sim)
    print*,"Input maps have been printed"
end if

! %EAC%: add filter to limit water table to evaporative layer
where(info_spat%wat_tab%mat<xml%depth%ze_fix .and. &
    info_spat%wat_tab%mat/=info_spat%wat_tab%header%nan)
    info_spat%wat_tab%mat=xml%depth%ze_fix
end where

! Initializes info_meteo matrices with meteorological data
call read_meteo_parameters(xml%sim,info_meteo,verbose)
print*, 'Variable "info_meteo" has been initialized'
if (.not. allocated(kcb_corr_fact)) call compute_kcb_corrections(kcb_corr_fact, crops, info_meteo, xml%sim)

! Initializes watsources and info_sources matrices
if (xml%sim%mode == 1) then         ! %AB% USE mode
    call init_water_sources_duty(xml,watsour,info_sources, info_meteo)
    print*, 'Variables "watsources" and "info_sources" have been initialized'
end if

! Soil-crop water balance algorithm
print*,"=== SIMULATION ==="
call simulation_manager(xml, info_spat, watsour, info_sources, info_meteo, crops, bare_soil,           &
                      & crop_rotations, kcb_corr_fact, crop_state, crop_yield_state, tab_CN2, tab_CN3, &
                      & theta2_rice, xml%sim%end, boundaries, verbose, summary, is_warmup=.false.      )

! Closes input files
if (xml%sim%mode == 1) call close_water_sources_dudy(info_sources,xml)        ! USE mode
call close_meteo_file(info_meteo)

! Memorization of time in which simulation ends
call date_and_time(values=t_stop)

! Prints simulation computing time
call print_execution_time(t_start, t_stop)

end program main

subroutine show_help()
print *, 'List of options:'
print *, '-h, -help: return some helpful information'
print *, '-d, -default: print default input values'
print *, '-p, -preview: show all parameters after reading configuration files'
print *, '-v, -verbose: print additional diagnostic messages'
print *, '-s, -summary: print only irrigation - both annual and periodical - outputs'
print *, '-f, -filename <parameters> : use file <parameters> to pass settings'
print *, '====================='
! stop execution because of a possible error
stop
end subroutine show_help

subroutine print_header()
! use makefile macros to set version and compilation timestamp
print *, '============== IDRAGRA - IDRologia AGRAria ================'
print *, 'contact: claudio.gandolfi@unimi.it'
print *, 'code version: ',GIT_VERSION
print *, 'compiled on: ',COMP_DATE
print *, 'source available on https://github.com/rita-tools/IdrAgra_v2'
print *, '============================================================'
print *, ''
end subroutine print_header

subroutine print_parameters(xml)
use mod_parameters, only: parameters
type(parameters), intent(in) :: xml

print *,'OutputPath = ',  xml%sim%path
print *,'MeteoPath = ', xml%sim%meteo_path
print *,'MeteoFileName = ', xml%sim%ws_list_fn
print *,'CropInputsFolder = ', xml%sim%crop_inputs_path
print *,'SoilUsesFilename = ', xml%sim%soil_uses_fn
print *,'CropFolder = ', xml%sim%crop_parameters_path
print *,'IrrMethPath = ', xml%sim%irr_met_path
print *,'IrrMethFileName = ', xml%sim%irr_met_list_fn
print *,'WatSourpath = ', xml%sim%watsour_path
print *,'InitialCondition = ', xml%sim%initial_condition
print *,'FinalCondition = ', xml%sim%final_condition
! spatial inputs
print *, 'domFileName = ', xml%sim%domain_fn
print *, 'tetaI_FCFileName = ', xml%sim%thetaI_FC_fn
print *, 'tetaII_FCFileName = ', xml%sim%thetaII_FC_fn
print *, 'tetaI_WPFileName = ', xml%sim%thetaI_WP_fn
print *, 'tetaII_WPFileName = ', xml%sim%thetaII_WP_fn
print *, 'tetaI_rFileName = ', xml%sim%thetaI_r_fn
print *, 'tetaII_rFileName = ', xml%sim%thetaII_r_fn
print *, 'tetaI_SATFileName = ', xml%sim%thetaI_SAT_fn
print *, 'tetaII_SATFileName = ', xml%sim%thetaII_SAT_fn
print *, 'slopeFileName = ', xml%sim%slope_fn
print *, 'drenFileName = ', xml%sim%dren_fn
print *, 'hydr_grFileName = ', xml%sim%hydr_group_fn
print *, 'ksat_IFileName = ', xml%sim%ksat_I_fn
print *, 'ksat_IIFileName = ', xml%sim%ksat_II_fn
print *, 'n_IFileName = ', xml%sim%n_I_fn
print *, 'n_IIFileName = ', xml%sim%n_II_fn
print *, 'thetaI_0FileName = ', xml%sim%thetaI_0_fn
print *, 'thetaII_0FileName = ', xml%sim%thetaII_0_fn
print *, 'paddy_soil_FileName = ', xml%sim%soil_prop_x_rice_fn
print *, 'eff_metodoFileName = ', xml%sim%eff_irr_fn
print *, 'eff_reteFileName = ', xml%sim%eff_net_fn
print *, 'irr_unitsFileName = ', xml%sim%irr_units_fn
print *, 'irr_methFileName = ', xml%sim%id_irr_meth_fn
print *, 'water_table_depth_FileName = ' , xml%sim%wat_table_fn
print *, 'ParRisCap_a3FileName = ', xml%sim%ParRisCap_a3_fn
print *, 'ParRisCap_a4FileName = ', xml%sim%ParRisCap_a4_fn
print *, 'ParRisCap_b1FileName = ', xml%sim%ParRisCap_b1_fn
print *, 'ParRisCap_b2FileName = ', xml%sim%ParRisCap_b2_fn
print *, 'ParRisCap_b3FileName = ', xml%sim%ParRisCap_b3_fn
print *, 'ParRisCap_b4FileName = ', xml%sim%ParRisCap_b4_fn
print *, 'ShapeAreaFlag = '       , xml%sim%f_shapearea
print *, 'ShapeAreaFileName = '   , xml%sim%shapearea_fn
print *, 'iRandomFileName = '     , xml%sim%irandom_fn

print *, 'mode = ',  xml%sim%mode
print *, 'mflag = ',  xml%sim%step_out
print *, 'output period = ', xml%sim%out_step_start, xml%sim%out_step_end, xml%sim%out_step_days
print *, 'InitialThetaFlag = ',  xml%sim%f_init_wc
print *, 'FinalThetaFlag = ',  xml%sim%f_theta_out
print *, 'RadnSowDaysWind = ',  xml%sim%sowing_range
print *, 'AggressiveSowing = ', xml%sim%aggressive_sowing
print *, 'ReadCropMaps = ', xml%sim%read_crop_maps
print *, 'CapillaryFlag = ', xml%sim%f_cap_rise
print *, 'UseModflowCoupling = ', xml%sim%use_modflow_coupling
print *, 'SoilUseVarFlag = ', xml%sim%f_soiluse
print *, 'MeteoStatWeightNum = ', xml%sim%n_interpolation_stations
print *, 'MeteoStatTotNum = ', xml%sim%n_weather_stations
print *, 'InterpolateTemperature = ', xml%sim%interpolate_temp
print *, 'InterpolateRain = ', xml%sim%interpolate_rain
print *, 'InterpolateHumidity = ', xml%sim%interpolate_hum
print *, 'InterpolateWind = ', xml%sim%interpolate_wind
print *, 'InterpolateRadiation = ', xml%sim%interpolate_rad
print *, 'forecast_day = ', xml%sim%forecast_day
print *, 'zEvap = ', xml%depth%ze_fix
print *, 'zRoot = ', xml%depth%zr_fix
print *, 'LambdaCN = ', xml%sim%lambda_cn
print *, 'StartIrrSeason = ', xml%sim%start_irr_season
print *, 'EndIrrSeason = ', xml%sim%end_irr_season
print *, 'h_maxpond', xml%sim%h_maxpond
end subroutine print_parameters
