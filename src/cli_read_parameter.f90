module cli_read_parameter
use mod_date, only: split_date
use mod_parameters
use mod_common
use mod_grid
use mod_irrigation, only: calc_perc_booster_pars
use cli_watsources
use mod_system
use mod_utility

use cli_save_outputs
implicit none

contains

subroutine read_all_parameters(file_xml, xml, verbose, reckless)
   character(len=*), intent(in) :: file_xml
    logical, intent(in) :: verbose, reckless
    type(parameters), intent(inout) :: xml
    ! Input related variables used in parsing loop
    integer :: ios          ! State variable (0 = ok)
    integer :: line, tablestart
    logical :: file_exists

    line = 0; tablestart = 0
    ios = 0

    call read_sim_parameters(file_xml, xml, reckless)

    inquire(file="cells.txt", exist=xml%sim%f_out_cells) ! update "output_cells"

    ! fw - evaporation !%AB% used for not irrigated cells or without irrigation events
    xml%irr%f_w = cost_fwEva

    ! allocate ET0 over the 24 hours
    allocate(xml%fet0(24))
    xml%ms_i%f_exists=.false. ; xml%ms_i%f_allocate_timeserie=.false. ;
    xml%ms_ii%f_exists=.false. ; xml%ms_ii%f_allocate_timeserie=.false. ;
    xml%intreu%f_exists=.false. ; xml%intreu%f_allocate_timeserie=.false. ;
    xml%cr%f_exists=.false. ; xml%cr%f_allocate_timeserie=.false. ;

    !!! %AB% check for time series of water releases
    inquire(file=trim(xml%sim%watsour_path)//trim(xml%sim%mon_sources_i_div_fn), exist=file_exists)    ! file_exists will be TRUE if the file exists
    if (file_exists .eqv. .true.) then
        xml%ms_i%f_exists=.true.
        xml%ms_i%f_allocate_timeserie=.true.
    end if

    inquire(file=trim(xml%sim%watsour_path)//trim(xml%sim%mon_sources_ii_div_fn), exist=file_exists)    ! file_exists will be TRUE if the file exists
    if (file_exists .eqv. .true.) then
        xml%ms_ii%f_exists=.true.
        xml%ms_ii%f_allocate_timeserie=.true.
    end if
    inquire(file=trim(xml%sim%watsour_path)//trim(xml%sim%int_reuse_div_fn), exist=file_exists)    ! file_exists will be TRUE if the file exists
    if (file_exists .eqv. .true.) then
        xml%intreu%f_exists=.true.
        xml%intreu%f_allocate_timeserie=.true.
    end if

    inquire(file=trim(xml%sim%watsour_path)//trim(xml%sim%cr_sources_list_fn), exist=file_exists)    ! file_exists will be TRUE if the file exists
    if (file_exists .eqv. .true.) then
        xml%cr%f_exists=.true.
        ! Collective unmonitored sources have no time series of discharges
    end if

    !%PS%: Don't require the irrigation methods file if in no-irrigation mode
    if (xml%sim%mode == 0) then
        xml%sim%n_irr_meth = 0
        allocate(xml%irr%met(0))
    else
        call read_all_irr_methods(xml, verbose)
    end if

end subroutine read_all_parameters


subroutine read_sim_parameters(file_xml, xml, reckless)
    ! read settings for the simulation
    character(len=*), intent(in) :: file_xml
    logical, intent(in) :: reckless
    type(parameters), intent(inout) :: xml

    integer :: unit_txt, p, ios, line
    character(len=300) :: buffer, label, dir_ic, dir_fc
    logical :: dir_exists

    line = 0
    ios = 0
    dir_ic = xml%sim%initial_condition
    dir_fc = xml%sim%final_condition
    open(newunit=unit_txt, file=trim(file_xml), status="old", action="read", iostat=ios)

    if (ios /= 0 ) then
        print *, 'Cannot open file ', trim(file_xml), '. The specified file does not exist.'
        stop
    end if

    do while (ios == 0)
        read(unit_txt, '(A)', iostat=ios) buffer
        if (ios == 0) then
            line = line + 1
            buffer = clean_input_line(buffer)
            ! if buffer is different from blank string, parse it
            if (buffer /= '') then
                call lower_case(buffer)
                p = scan(buffer, '=')
                label = buffer(1:p-1)
                buffer = buffer(p+1:)
                select case (label)
                    case ('outputpath')
                        read(buffer, *, iostat=ios) xml%sim%path
                        xml%sim%path = replace_str( string = xml%sim%path &
                                            , search = "\\" &
                                            , substitute = delimiter &
                                            )
                        inquire(file=trim(xml%sim%path), exist=dir_exists)   ! dir_exists will be TRUE if the directory exists
                        if (dir_exists .and. .not. reckless) then
                            print *,'The directory ', trim(xml%sim%path), ' already exists and will be updated'
                            print *, " <enter> to continue "
                            read *
                        else
                            call make_dir(xml%sim%path)
                        end if
                    case ('inputpath') ! path to spazialized input files
                        read(buffer, *, iostat=ios) xml%sim%input_path
                        xml%sim%input_path = replace_str( string = xml%sim%input_path &
                                            , search = "\\" &
                                            , substitute = delimiter &
                                            )
                    case ('meteopath') ! path to meteo files
                        read(buffer, *, iostat=ios) xml%sim%meteo_path
                        xml%sim%meteo_path = replace_str( string = xml%sim%meteo_path &
                                            , search = "\\" &
                                            , substitute = delimiter &
                                            )
                    case ('meteofilename') ! name of the file with the list of weather station
                        read(buffer, *, iostat=ios) xml%sim%ws_list_fn
                    case ('cropinputsfolder')
                        read(buffer, *, iostat=ios) xml%sim%crop_inputs_path
                        xml%sim%crop_inputs_path = replace_str(xml%sim%crop_inputs_path, "\\", delimiter)
                    case ('soilusesfilename')
                        read(buffer, *, iostat=ios) xml%sim%soil_uses_fn
                    case ('co2filename')
                        read(buffer, *, iostat=ios) xml%sim%co2_conc_fn
                    case ('cropfolder')
                        read(buffer, *, iostat=ios) xml%sim%crop_parameters_path
                        xml%sim%crop_parameters_path = replace_str(xml%sim%crop_parameters_path, "\\", delimiter)
                    case ('irrmethpath') ! path to meteo files
                        read(buffer, *, iostat=ios) xml%sim%irr_met_path
                        xml%sim%irr_met_path = replace_str( string = xml%sim%irr_met_path &
                                            , search = "\\" &
                                            , substitute = delimiter &
                                            )
                    case ('irrmethfilename') ! name of the file with the list of weather station
                        read(buffer, *, iostat=ios) xml%sim%irr_met_list_fn
                    case ('watsourpath') ! path to water sources subfolder
                        read(buffer, *, iostat=ios) xml%sim%watsour_path
                        xml%sim%watsour_path = replace_str( string = xml%sim%watsour_path &
                                            , search = "\\" &
                                            , substitute = delimiter &
                                            )
                    case ('mode') ! running mode: 0 - no irrigation; 1 - consumption; 2 - water needs to field capacity; 3 - water needs with fixed volume
                        read(buffer, *, iostat=ios) xml%sim%mode
                    case ('initialthetaflag') ! T: read init moisture from external file
                        read(buffer, *, iostat=ios) xml%sim%f_init_wc
                        if (xml%sim%f_init_wc) then
                            print *, "Warning: since the removal of cropcoef (IdrAgra 3.0.0 onwards), it is recommended to set InitialThetaFlag = F."
                            print *, "Because you have set it to 'T' instead, note that crop state in the first simulation year might not be &
                                    & representative of actual field conditions (crops can't be sown before the simulation starts)"
                            if (.not. reckless) then
                                print *, "press <enter> to continue "
                                read *
                            end if
                        end if
                    case ('finalthetaflag') ! T: if final soil moisture condition is written on files, F if not
                        read(buffer, *, iostat=ios) xml%sim%f_theta_out
                    case ('startsimulation')    ! date of start simulation
                        read (buffer, *, iostat=ios)
                        call split_date(buffer, xml%sim%start)
                    case ('endsimulation')    ! date of end simulation
                        read (buffer, *, iostat=ios)
                        call split_date(buffer, xml%sim%end)
                    case ('initialconditionpath') ! path to initial condition input files
                        read(buffer, *, iostat=ios) xml%sim%initial_condition
                        xml%sim%initial_condition = replace_str( string = xml%sim%initial_condition &
                                            , search = "\\" &
                                            , substitute = delimiter &
                                            )
                        inquire(FILE=trim(xml%sim%initial_condition), EXIST=dir_exists)   ! dir_exists will be TRUE if the directory exists
                        if (dir_exists .eqv. .false.) then
                            print *,'The directory ', trim(xml%sim%initial_condition),&
                            ' does not exist. No initial condition can be uploaded.'
                            print *, 'Execution will be aborted...'
                            stop
                        end if
                    case ('initialcondition') ! initial condition input filenames (root)
                        read(buffer, *, iostat=ios) xml%sim%thetaI_0_fn
                        xml%sim%thetaI_0_fn = trim(xml%sim%thetaI_0_fn)//'I'
                        read(buffer, *, iostat=ios) xml%sim%thetaII_0_fn
                        xml%sim%thetaII_0_fn = trim(xml%sim%thetaII_0_fn)//'II'
                    case ('finalconditionpath') ! path to final condition input files
                        read(buffer, *, iostat=ios) xml%sim%final_condition
                        xml%sim%final_condition = replace_str( string = xml%sim%final_condition &
                                            , search = "\\" &
                                            , substitute = delimiter &
                                            )
                        inquire(FILE=trim(xml%sim%final_condition), EXIST=dir_exists)   ! dir_exists will be TRUE if the directory exists
                        if (dir_exists .eqv. .true.) then
                            print *,'The directory ', trim(xml%sim%final_condition), ' already exists and will be updated'
                        else
                            call make_dir(xml%sim%final_condition)
                        end if
                    case ('finalcondition') ! final condition input filenames (root)
                        read(buffer, *, iostat=ios) xml%sim%thetaI_end_fn
                        xml%sim%thetaI_end_fn = trim(xml%sim%thetaI_end_fn)//'I'
                        read(buffer, *, iostat=ios) xml%sim%thetaII_end_fn
                        xml%sim%thetaII_end_fn = trim(xml%sim%thetaII_end_fn)//'II'
                    case ('capillaryflag') ! T: calculate capillary rise
                        read(buffer, *, iostat=ios) xml%sim%f_cap_rise
                    case('soilusevarflag') ! T: land use update yearly
                        read(buffer, *, iostat=ios) xml%sim%f_soiluse
                    case ('meteostatweightnum') ! Number of weather stations used to interpolate each cell's weather & phenology
                        read(buffer, *, iostat=ios)xml%sim%n_interpolation_stations
                    case ('meteostattotnum') ! number of areas of weather stations TODO: check
                        read(buffer, *, iostat=ios)xml%sim%n_weather_stations
                    case ('interpolatetemperature')
                        read(buffer, *, iostat=ios) xml%sim%interpolate_temp
                    case ('interpolaterain')
                        read(buffer, *, iostat=ios) xml%sim%interpolate_rain
                    case ('interpolatehumidity')
                        read(buffer, *, iostat=ios) xml%sim%interpolate_hum
                    case ('interpolatewind')
                        read(buffer, *, iostat=ios) xml%sim%interpolate_wind
                    case ('interpolateradiation')
                        read(buffer, *, iostat=ios) xml%sim%interpolate_rad
                    case ('monthlyflag') !%PS%, todo: rename or add aliases
                        select case (trim(adjustl(buffer)))
                            case ('monthly', 'month', 'm', 't')
                                xml%sim%step_out = output_monthly
                            case ('weekly', 'week', 'w')
                                xml%sim%step_out = output_weekly
                            case ('periodic', 'f', 'fixed')
                                xml%sim%step_out = output_periodic
                            case default
                                xml%sim%step_out = output_monthly
                                print *, 'Unrecognized output timestep option "', trim(adjustl(buffer)),'". Will use "monthly".'
                                read *
                        end select
                    case ('weekday')
                        select case (trim(adjustl(buffer)))
                            case ('monday', 'mon', '1')
                                xml%sim%weekly_output_weekday = 1
                            case ('tuesday', 'tue', '2')
                                xml%sim%weekly_output_weekday = 2
                            case ('wednesday', 'wed', '3')
                                xml%sim%weekly_output_weekday = 3
                            case ('thursday', 'thu', '4')
                                xml%sim%weekly_output_weekday = 4
                            case ('friday', 'fri', '5')
                                xml%sim%weekly_output_weekday = 5
                            case ('saturday', 'sat', '6')
                                xml%sim%weekly_output_weekday = 6
                            case ('sunday', 'sun', '7')
                                xml%sim%weekly_output_weekday = 7
                            case default
                                xml%sim%weekly_output_weekday = 1
                                print *, "Weekly output is printed each Monday"
                                read *
                        end select
                    case ('startdate')
                        read(buffer, *, iostat=ios) xml%sim%out_step_start
                    case ('enddate')
                        read(buffer, *, iostat=ios) xml%sim%out_step_end
                    case ('deltadate')
                        read(buffer, *, iostat=ios) xml%sim%out_step_days
                    case ('randsowdayssym')
                        select case (trim(adjustl(buffer)))
                            case ('symmetric', 's', '1', 't', 'true')
                                xml%sim%rand_symmetry = .true.
                            case ('asymmetric', 'as', '0', 'f', 'false')
                                xml%sim%rand_symmetry = .false.
                            case default
                                xml%sim%rand_symmetry = .true.
                                print *, "Emergence date is distributed symmetrically"
                                read *
                        end select
                    case ('randsowdayswind') ! range of sowinf
                        read(buffer, *, iostat=ios) xml%sim%sowing_range
                    case ('aggressivesowing')
                        read(buffer, *, iostat=ios) xml%sim%aggressive_sowing
                    case ('repeatable') ! set simulation with random seeding repeatible
                        if ((trim(adjustl(buffer)) == 'true') .or. (trim(adjustl(buffer))== 't') .or. (trim(adjustl(buffer)) == '1'))  then
                            xml%sim%repeatable = .true.
                        else
                            xml%sim%repeatable = .false.
                        end if
                    case ('forecast_day') ! Number days to consider for cumulative precipitation
                        read(buffer, *, iostat=ios)xml%sim%forecast_day
                    ! parameter for percolation booster
                    case ('01q_eva')  ! 10° percentile of the first layer
                        read(buffer, *, iostat=ios)xml%sim%quantiles(1,1)
                    case ('09q_eva')  ! 90° percentile of the first layer
                        read(buffer, *, iostat=ios)xml%sim%quantiles(1,2)
                    case ('01q_trasp')  ! 10° percentile of the second layer
                        read(buffer, *, iostat=ios)xml%sim%quantiles(2,1)
                    case ('09q_trasp')  ! 90° percentile of the second layer
                        read(buffer, *, iostat=ios)xml%sim%quantiles(2,2)
                    case ('startirrseason') ! start doy of irrigation season
                        read(buffer, *, iostat=ios)xml%sim%start_irr_season
                    case ('endirrseason') ! end doy of irrigation season
                        read(buffer, *, iostat=ios)xml%sim%end_irr_season
                    case ('lim_prec') ! Minimum meaningfull precipitation
                        read(buffer, *, IOSTAT=ios)xml%sim%h_prec_lim
                    case ('zevap') ! Ze_fix: thickness of the first layer
                        read(buffer, *, iostat=ios)xml%depth%ze_fix
                    case ('zroot') ! Zr_fix: default thickness of the second layer
                        read(buffer, *, iostat=ios)xml%depth%zr_fix
                    case ('lambdacn') ! lambda_CN: coefficient for calculate the initial abstraction
                        read(buffer, *, iostat=ios)xml%sim%lambda_cn
                    ! add new case to edit input files for irrigation
                    case ('watsources_fn') ! list of water sources for each irrigation units
                        read(buffer, *, iostat=ios) xml%sim%watsources_fn
                    case ('mon_sources_i_div_fn') ! dicharges series from superficial diversion
                        read(buffer, *, iostat=ios) xml%sim%mon_sources_i_div_fn
                    case ('mon_sources_ii_div_fn') ! dicharges series from superficial sources
                        read(buffer, *, iostat=ios) xml%sim%mon_sources_ii_div_fn
                    case ('int_reuse_div_fn') ! dicharges series from internal reuse
                        read(buffer, *, iostat=ios) xml%sim%int_reuse_div_fn
                    case ('cr_sources_list_fn') ! dicharges series from collective runtime sources
                        read(buffer, *, iostat=ios) xml%sim%cr_sources_list_fn
                    case ('irrdistr_list_fn') ! discharges distribution in irrigation units
                        read(buffer, *, iostat=ios) xml%sim%irrdistr_list_fn
                    case ('sched_irr_fn') ! distribution rules for scheduled irrigation
                        read(buffer, *, iostat=ios) xml%sim%sched_irr_fn

                    case('h_maxpond')
                        read(buffer, *, iostat=ios) xml%sim%h_maxpond

                    case('fc_ratio')
                        read(buffer, *, iostat=ios) xml%sim%fc_ratio

                    ! set general options
                    case ('prt_all')
                        read(buffer, *, iostat=ios) xml%sim%prt_all
                        ! call the function to set print option on/off (y/n)
                        call print_all(xml%sim%prt_all, xml%sim)
                    case ('prt_step')
                        read(buffer, *, iostat=ios) xml%sim%prt_step
                        ! call the function to set print option on/off (y/n)
                        call print_step(xml%sim%prt_step, xml%sim)
                    case ('prt_annual')
                        read(buffer, *, iostat=ios) xml%sim%prt_annual
                        ! call the function to set print option on/off (y/n)
                        call print_annual(xml%sim%prt_annual, xml%sim)
                    case ('prt_yield')
                        read(buffer, *, iostat=ios) xml%sim%prt_yield
                        ! call the function to set print option on/off (y/n)
                        call print_yield(xml%sim%prt_yield, xml%sim)
                    case ('prt_debug')
                        read(buffer, *, iostat=ios) xml%sim%prt_debug
                        ! call the function to set print option on/off (y/n)
                        call print_debug(xml%sim%prt_debug, xml%sim)
                    case ('prt_cell_all')
                        read(buffer, *, iostat=ios) xml%sim%prt_cell_all
                        ! call the function to set print option on/off (y/n)
                        call print_cell_all(xml%sim%prt_cell_all, xml%sim)
                    ! set specific options for printing
                    case ('prt_stp_rain')
                        read(buffer, *, iostat=ios) xml%sim%prt_stp_rain
                    case ('prt_stp_transp_act')
                        read(buffer, *, iostat=ios) xml%sim%prt_stp_transp_act
                    case ('prt_stp_transp_pot')
                        read(buffer, *, iostat=ios) xml%sim%prt_stp_transp_pot
                    case ('prt_stp_irr')
                        read(buffer, *, iostat=ios) xml%sim%prt_stp_irr
                    case ('prt_stp_irr_loss')
                        read(buffer, *, iostat=ios) xml%sim%prt_stp_irr_loss
                    case ('prt_stp_cap_rise')
                        read(buffer, *, iostat=ios) xml%sim%prt_stp_cap_rise
                    case ('prt_stp_irr_nm_priv')
                        read(buffer, *, iostat=ios) xml%sim%prt_stp_irr_nm_priv
                    case ('prt_stp_irr_nm_col')
                        read(buffer, *, iostat=ios) xml%sim%prt_stp_irr_nm_col
                    case ('prt_stp_deep_perc')
                        read(buffer, *, iostat=ios) xml%sim%prt_stp_deep_perc
                    case ('prt_stp_runoff')
                        read(buffer, *, iostat=ios) xml%sim%prt_stp_runoff
                    case ('prt_stp_et_pot')
                        read(buffer, *, iostat=ios) xml%sim%prt_stp_et_pot
                    case ('prt_stp_et_act')
                        read(buffer, *, iostat=ios) xml%sim%prt_stp_et_act
                    case ('prt_dbg_eva_act')
                        read(buffer, *, iostat=ios) xml%sim%prt_dbg_eva_act
                    case ('prt_dbg_eff_rain')
                        read(buffer, *, iostat=ios) xml%sim%prt_dbg_eff_rain
                    case ('prt_dbg_perc1')
                        read(buffer, *, iostat=ios) xml%sim%prt_dbg_perc1
                    case ('prt_dbg_perc2')
                        read(buffer, *, iostat=ios) xml%sim%prt_dbg_perc2
                    case ('prt_dbg_h_soil1')
                        read(buffer, *, iostat=ios) xml%sim%prt_dbg_h_soil1
                    case ('prt_dbg_h_soil2')
                        read(buffer, *, iostat=ios) xml%sim%prt_dbg_h_soil2
                    case ('prt_yr_rain')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_rain
                    case ('prt_yr_rain_crop_season')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_rain_crop_season
                    case ('prt_yr_irr')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_irr
                    case ('prt_yr_irr_loss')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_irr_loss
                    case ('prt_yr_eva_act_crop_season')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_eva_act_crop_season
                    case ('prt_yr_eva_pot_crop_season')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_eva_pot_crop_season
                    case ('prt_yr_transp_act')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_transp_act
                    case ('prt_yr_transp_pot')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_transp_pot
                    case ('prt_yr_runoff')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_runoff
                    case ('prt_yr_net_flux_gw')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_net_flux_gw
                    case ('prt_yr_total_eff')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_total_eff
                    case ('prt_yr_n_irr_events')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_n_irr_events
                    case ('prt_yr_h_irr_mean')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_h_irr_mean
                    case ('prt_yr_biomass_pot')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_biomass_pot
                    case ('prt_yr_yield_pot')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_yield_pot
                    case ('prt_yr_yield_act')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_yield_act
                    case ('prt_yr_T_act_sum')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_T_act_sum
                    case ('prt_yr_T_pot_sum')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_T_pot_sum
                    case ('prt_yr_f_WS_stage')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_f_WS_stage
                    case ('prt_yr_f_WS')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_f_WS
                    case ('prt_yr_f_HS')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_f_HS
                    case ('prt_yr_f_HS_sum')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_f_HS_sum
                    case ('prt_yr_dbg_eva_act_tot')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_dbg_eva_act_tot
                    case ('prt_yr_dbg_rain_eff')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_dbg_rain_eff
                    case ('prt_yr_dbg_iter1')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_dbg_iter1
                    case ('prt_yr_dbg_iter2')
                        read(buffer, *, iostat=ios) xml%sim%prt_yr_dbg_iter2

                    ! cells output
                    case ('prt_cell_convergence')
                        read(buffer, *, iostat=ios) xml%sim%prt_cell_convergence
                    case ('prt_cell_evaporation')
                        read(buffer, *, iostat=ios) xml%sim%prt_cell_evaporation
                    case ('prt_cell_runoff')
                        read(buffer, *, iostat=ios) xml%sim%prt_cell_runoff
                    case ('prt_cell_et0')
                        read(buffer, *, iostat=ios) xml%sim%prt_cell_et0

                    ! not classified debug outputs
                    case ('prt_debug_out')
                        read(buffer, *, iostat=ios) xml%sim%prt_debug_out

                    case ('prt_init_cond')
                        read(buffer, *, iostat=ios) xml%sim%prt_init_cond


                    case default ! all other cases ...
                        print *, 'Skipping invalid or obsolete label <',trim(label),'> at line', line, &
                            & ' of file: ', trim(file_xml)
                end select
            end if
        end if
    end do

    if (xml%sim%initial_condition == dir_ic .and. xml%sim%input_path /= dir_ic) then
        xml%sim%initial_condition = xml%sim%input_path
    end if
    if (xml%sim%final_condition == dir_fc .and. xml%sim%path /= dir_fc) then
        xml%sim%final_condition = xml%sim%path
    end if

    close(unit_txt)
end subroutine read_sim_parameters

! TODO: %EAC% at the moment the id of the irrigation method define its order in the list
subroutine read_irr_method(irr_method_fn, met, verbose)
    character(len=*),intent(in) :: irr_method_fn
    type(par_method),dimension(:),intent(inout) :: met
    logical, intent(in), optional :: verbose
    ! Input related variables used in parsing loop
    character(len=300) :: comment,buffer, label
    integer :: c
    integer :: p
    character(len=300)::i
    integer :: unit_txt
    integer :: line
    integer :: ios      ! state variable (0 = ok)
    integer :: num, e
    line = 0
    ios = 0
    e = 1
    open(newunit=unit_txt, file=trim(irr_method_fn), action="read", iostat=ios)
    if (ios /= 0 ) then
        print *, "Cannot open file ", trim(irr_method_fn), ". The specified file does not exist."
        print *, 'Execution will be aborted...'
        stop
    end if
    do while (ios == 0)
        read (unit_txt, '(A)', iostat = ios) buffer
        if (ios == 0) then
            line = line +1
            buffer = trim(buffer)
            c = scan(buffer, '#')
            if (c /= 0) then
                comment = buffer(c+1:)
                buffer = buffer(1:c-1)
            end if
            if (buffer /= '') then
                call lower_case(buffer)
                c = scan(buffer, '=')
                label = buffer(1:c-1)
                buffer = buffer(c+1:)

                select case (label)
                    case ('id')
                        read (buffer, *, iostat = ios) p
                        if (p > size(met, 1)) then
                            print *, 'The irrigation method Id, ', p, ', in the file "', trim(irr_method_fn), &
                                & '", is above upper bound of the number of irrigation methods: ', size (met, 1)
                            print *, 'Execution will be aborted...'
                            stop
                        end if
                        allocate(met(p)%freq(24))
                        read (buffer, *, iostat = ios) met(p)%id  ! id of the irrigation method
                        met(p)%f_wet = cost_fwEva ! set to the default value
                    case ('qwat','qadaq')
                        read (buffer, *, iostat = ios) met(p)%h_irr ! irrigation depth [mm]
                    case ('fw')
                        read (buffer, *, iostat = ios) met(p)%f_wet    ! soil wetted fraction [-][0-1]
                    case ('k_stress','irr_th_mn')
                        read (buffer, *, iostat = ios) met(p)%irr_th_ms ! fraction of RAW that activate irrigation for monitored sources
                    case ('k_stresswells', 'irr_th_unm')
                        read (buffer, *, iostat = ios) met(p)%irr_th_unm ! fraction of RAW that activate irrigation for unmonitored sources
                    case ('min_a')
                        read (buffer, *, iostat = ios) met(p)%a_min ! min value of the "a" parameter for percolation model
                    case ('max_a')
                        read (buffer, *, iostat = ios)  met(p)%a_max ! max value of the "a" parameter for percolation model
                    case ('min_b')
                        read (buffer, *, iostat = ios) met(p)%b_min ! min value of the "b" parameter for percolation model
                    case ('max_b')
                        read (buffer, *, iostat = ios)  met(p)%b_max ! max value of the "b" parameter for percolation model
                    case ('interceptionflag') ! Flag interception
                        read (buffer, *, IOSTAT = ios) i
                        if ((i == 'true') .or. (i == 't'))  then
                            met(p)%f_interception = 1
                        else
                            met(p)%f_interception = 0
                        end if
                    ! irrigation losses model parameters
                    case ('a')
                        read (buffer, *, iostat = ios)  met(p)%a_loss ! a
                    case ('b')
                        read (buffer, *, iostat = ios)  met(p)%b_loss ! b
                    case ('c')
                        read (buffer, *, iostat = ios)  met(p)%c_loss ! c
                    case ('irr_starts')
                        read (buffer, *, iostat = ios)  met(p)%irr_starts ! doy when irrigation season starts
                    case ('irr_ends')
                        read (buffer, *, iostat = ios)  met(p)%irr_ends ! doy when irrigation season starts
                    case ('h_maxpond')
                        read (buffer, *, iostat = ios)  met(p)%h_maxpond ! maximum ponding depth
                    case default
                        read (label, *, iostat = e) num
                        if ((e==0) .and. (num>=1) .and. (num<=24)) then
                            read (buffer, *, iostat = ios) met(p)%freq(num)
                        else
                            print *, 'Skipping invalid label <',trim(label),'> at line', line, ' of file: ', trim(irr_method_fn)
                            print *, 'Execution will be aborted...'
                            stop
                        end if
                end select
            end if
        end if
    end do
    close(unit_txt)

    if (verbose .eqv. .true.) then
        print *,'====== DEBUG: irrigation method: ', irr_method_fn, ' ====='
        print *,'Id = ',  p
        print *,'Qwat = ',  met(p)%h_irr
        print *,'fw = ', met(p)%f_wet
        print *,'Irrigation threshold monitored sources = ',  met(p)%irr_th_ms
        print *,'Irrigation threshold unmonitored sources = ',  met(p)%irr_th_unm
        print *,'min_a = ', met(p)%a_min
        print *,'max_a = ',  met(p)%a_max
        print *,'min_b = ', met(p)%b_min
        print *,'max_b = ',  met(p)%b_max
        if (met(p)%f_interception == 1)  then
            print *, 'InterceptionFlag = TRUE'
        else
            print *, 'InterceptionFlag = FALSE'
        end if
        print *,'a = ',  met(p)%a_loss
        print *,'b = ', met(p)%b_loss
        print *,'c = ', met(p)%c_loss
        print *,'freqirr = ', met(p)%freq
        print *,'===== END DEBUG ====='
    end if
end subroutine read_irr_method

subroutine read_all_irr_methods(xml, debug)
    logical, intent(in) :: debug
    type(parameters), intent(inout) :: xml
    integer :: free_unit
    integer :: i
    character(len=200) :: irr_method_fn
    ! Input related variables used in parsing loop
    character(len=300) :: comment,buffer, label
    integer :: p
    integer :: ios          ! state variable (0 = ok)
    integer :: line, tablestart

    line = 0; tablestart = 0
    ios = 0
    print*,'irr_met_path: ',     xml%sim%irr_met_path
    print*,'irr_met_filename: ', xml%sim%irr_met_list_fn

    open(newunit=free_unit, file=trim(xml%sim%irr_met_path)//trim(xml%sim%irr_met_list_fn), action="read")
    line = 0; tablestart = 0
    ios = 0
    do while (ios == 0)
        read(free_unit, '(A)', iostat=ios) buffer
        if (ios == 0) then
            line = line + 1
            buffer = trim(buffer)
            p = scan(buffer, '#')
            if (p /= 0) then
                comment = buffer(p+1:)
                buffer = buffer(1:p-1)
            end if
            if (buffer /= '') then
                call lower_case(buffer)
                p = scan(buffer, '=')
                label = buffer(1:p-1)
                buffer = buffer(p+1:)
                select case (label)
                    case ('irrmethnum')
                         ! number of irrigation method considered in the simulation
                        read(buffer, *, iostat=ios) xml%sim%n_irr_meth
                        allocate (xml%irr%met(xml%sim%n_irr_meth))
                        ! set default values for irrigation methods from general setup
                        do i = 1, xml%sim%n_irr_meth
                            xml%irr%met(i)%irr_starts = xml%sim%start_irr_season
                            xml%irr%met(i)%irr_ends = xml%sim%end_irr_season
                        end do
                    case ('list')
                        tablestart = line
                        ! List of the file with the irrigation methods
                        do i = 1, xml%sim%n_irr_meth
                            read(free_unit, *, iostat=ios) irr_method_fn
                            line = line + 1
                            call read_irr_method(trim(xml%sim%irr_met_path)//irr_method_fn, xml%irr%met, debug)
                        end do
                    case ('endlist')
                        if ((line - tablestart - 1) > xml%sim%n_irr_meth) then
                            stop 'Irrigation methods number (IrrMethNum) is higher than irrigation methods listed'
                        else if ((line - tablestart - 1) < xml%sim%n_irr_meth) then
                            stop 'Irrigation methods number (IrrMethNum) is lower than irrigation methods listed'
                        end if
                    case default ! all other cases ...
                        print *, 'Skipping invalid label <',trim(label),'> at line ', line, ' of file: ', &
                            & trim(xml%sim%irr_met_list_fn)
                        print *, 'Execution will be aborted...'
                        stop
                end select
            end if
        end if
    end do
    close (free_unit)

end subroutine read_all_irr_methods


subroutine read_spatial_info(info_spat, extent, sim, tab_CN2, tab_CN3, theta2_rice, met)
    ! read all spatially distributed parameters and check parameters
    ! init tab_CN TODO: separate the function

    type(bound),intent(in)::extent
    type(simulation),intent(inout)::sim
    type(par_method),dimension(:),intent(inout)::met
    type(spatial_info),intent(out)::info_spat
    real(dp),dimension(:,:,:),intent(out):: tab_CN2, tab_CN3
    integer::ios
    type(soil2_rice)::theta2_rice
    integer :: line

    line = 0
    ios = 0

    call read_grid_files(info_spat, extent, sim)
    call check_grid(info_spat, sim)

    select case (sim%mode)
        case (0)
            print *, 'Irrigation distribution parameters are not set'
        case (1:4)
            call read_irr_grid(info_spat, extent, sim)
            call check_irr_grid(info_spat, sim)
            call calc_perc_booster_pars(info_spat, met, sim%quantiles)
        case default
            stop 'Simulation mode has been incorrectly set'
    end select

    call read_rice_parameters(sim, theta2_rice)
    call init_cn_table(tab_CN2, tab_CN3)

end subroutine read_spatial_info

subroutine read_grid_files(info_spat, extent, sim)
    !Read *.asc files content into info_spat variable
    type(bound),intent(in)::extent
    type(simulation),intent(inout)::sim
    type(spatial_info),intent(out)::info_spat
    integer::k
    character(len=30)::k_str
    character(len=300)::dir, dir_ic
    character(len=30)::start_year

    dir = sim%input_path
    dir_ic = sim%initial_condition

    call read_grid(trim(dir)//trim(sim%domain_fn)//'.asc',info_spat%domain,sim,extent)
    ! Water content parameters
    call read_grid(trim(dir)//trim(sim%thetaI_SAT_fn)//'.asc',info_spat%theta(1)%sat,sim,extent);
    call read_grid(trim(dir)//trim(sim%thetaII_SAT_fn)//'.asc',info_spat%theta(2)%sat,sim,extent);
    call read_grid(trim(dir)//trim(sim%thetaI_FC_fn)//'.asc',info_spat%theta(1)%fc,sim,extent)
    call read_grid(trim(dir)//trim(sim%thetaII_FC_fn)//'.asc',info_spat%theta(2)%fc,sim,extent)
    call read_grid(trim(dir)//trim(sim%thetaI_WP_fn)//'.asc',info_spat%theta(1)%wp,sim,extent)
    call read_grid(trim(dir)//trim(sim%thetaII_WP_fn)//'.asc',info_spat%theta(2)%wp,sim,extent)
    call read_grid(trim(dir)//trim(sim%thetaI_r_fn)//'.asc',info_spat%theta(1)%r,sim,extent)
    call read_grid(trim(dir)//trim(sim%thetaII_r_fn)//'.asc',info_spat%theta(2)%r,sim,extent)
    ! Brooks and Corey's equation parameters
    call read_grid(trim(dir)//trim(sim%ksat_I_fn)//'.asc',info_spat%k_sat(1),sim,extent)
    call read_grid(trim(dir)//trim(sim%ksat_II_fn)//'.asc',info_spat%k_sat(2),sim,extent)
    call read_grid(trim(dir)//trim(sim%n_I_fn)//'.asc',info_spat%fact_n(1),sim,extent)
    call read_grid(trim(dir)//trim(sim%n_II_fn)//'.asc',info_spat%fact_n(2),sim,extent)
    ! CN method parameters: slope, hydrologic condition (dren) and hydrologic soil group (hydr_gr)
    call read_grid(trim(dir)//trim(sim%slope_fn)//'.asc',info_spat%slope,sim,extent)
    info_spat%slope%mat = info_spat%slope%mat/100  ! Slopes are converted to percentage: x/100 = 0.x
    call read_grid(trim(dir)//trim(sim%dren_fn)//'.asc',info_spat%drainage,sim,extent)
    call read_grid(trim(dir)//trim(sim%hydr_group_fn)//'.asc',info_spat%hydr_gr,sim,extent)

    ! read initial moisture condition
    if(sim%f_init_wc .eqv. .true.)then  ! from external files
        call read_grid(trim(dir_ic)//trim(sim%thetaI_0_fn)//'.asc',info_spat%theta(1)%old,sim,extent)
        call read_grid(trim(dir_ic)//trim(sim%thetaII_0_fn)//'.asc',info_spat%theta(2)%old,sim,extent)
    else ! Warmup starts at FC moisture; first actual year overwrites this using warmup_theta later 
        call read_grid(trim(dir)//trim(sim%thetaI_FC_fn)//'.asc',info_spat%theta(1)%old,sim,extent)
        call read_grid(trim(dir)//trim(sim%thetaII_FC_fn)//'.asc',info_spat%theta(2)%old,sim,extent)
    end if

    ! read initial land use map
    if (sim%f_soiluse .eqv. .false.)then  ! Static land use map
        call read_grid(trim(dir)//trim(sim%soiluse_fn)//'.asc',info_spat%soil_use_id,sim,extent)
        call overlay_domain(info_spat%soil_use_id, info_spat%domain, sim%soiluse_fn)
    else                                ! Dynamic land use map
        write (start_year, *) sim%start_year
        call read_grid(trim(dir)//trim(sim%soiluse_fn)//'_'//trim(adjustl(start_year))//'.asc', &
            & info_spat%soil_use_id,sim,extent)
        print *, "First year soil use has been read"
    end if

    ! read parameters for capillary rise and water table depth
    if(sim%f_cap_rise .eqv. .true.)then
        call read_grid(trim(dir)//trim(sim%wat_table_fn)//'.asc',info_spat%wat_tab,sim,extent)
        call read_grid(trim(dir)//trim(sim%ParRisCap_a3_fn)//'.asc',info_spat%a3,sim,extent)
        call read_grid(trim(dir)//trim(sim%ParRisCap_a4_fn)//'.asc',info_spat%a4,sim,extent)
        call read_grid(trim(dir)//trim(sim%ParRisCap_b1_fn)//'.asc',info_spat%b1,sim,extent)
        call read_grid(trim(dir)//trim(sim%ParRisCap_b2_fn)//'.asc',info_spat%b2,sim,extent)
        call read_grid(trim(dir)//trim(sim%ParRisCap_b3_fn)//'.asc',info_spat%b3,sim,extent)
        call read_grid(trim(dir)//trim(sim%ParRisCap_b4_fn)//'.asc',info_spat%b4,sim,extent)
    else
        info_spat%wat_tab = info_spat%domain
        info_spat%wat_tab%mat=100
        info_spat%a3 = info_spat%domain
        info_spat%a4 = info_spat%domain
        info_spat%b1 = info_spat%domain
        info_spat%b2 = info_spat%domain
        info_spat%b3 = info_spat%domain
        info_spat%b4 = info_spat%domain
    end if

    ! read weather station weights
    allocate(info_spat%weight_ws(sim%n_interpolation_stations))
    do k=1,size(info_spat%weight_ws)
        write(k_str,*) k
        call read_grid(trim(dir)//trim(sim%meteoweight_fn)//'_'//trim(adjustl(k_str))//'.asc',&
            & info_spat%weight_ws(k),sim, extent)
    end do

    ! read shape area, if the file exists
    inquire (file=trim(dir)//trim(sim%shapearea_fn)//'.asc', exist=sim%f_shapearea)
    if(sim%f_shapearea .eqv. .true.) then
        call read_grid(trim(dir)//trim(sim%shapearea_fn)//'.asc', info_spat%cell_area,sim,extent)
    end if

    ! %EAC%: init irandom if exists
    inquire (file=trim(dir)//trim(sim%irandom_fn)//'.asc', exist=sim%f_irandom)
    if (sim%f_irandom .eqv. .true.) then
        print *,'Init irandom: ', trim(dir)//trim(sim%irandom_fn)//'.asc'
        call read_grid(trim(dir)//trim(sim%irandom_fn)//'.asc', info_spat%irandom,sim,extent)
    else
        ! init irandom matrix
        info_spat%irandom = info_spat%domain
    end if


end subroutine read_grid_files

subroutine read_irr_grid(info_spat, extent, sim)
    ! distributed parameters for irrigation
    type(bound),intent(in)::extent
    type(simulation),intent(inout)::sim
    type(spatial_info),intent(inout)::info_spat
    character(len=300)::dir
    character(len=30)::start_year

    dir = sim%input_path

    select case (sim%mode)
        case (1)
            ! read parameter for irrigation practice
            call read_grid(trim(dir)//trim(sim%irr_units_fn)//'.asc',info_spat%irr_unit_id,sim,extent)
            if (sim%f_soiluse .eqv. .false.) then
                call read_grid(trim(dir)//trim(sim%id_irr_meth_fn)//'.asc',info_spat%irr_meth_id,sim,extent)
            else
                write(start_year,*) sim%start_year
                call read_grid (trim(dir)//trim(sim%id_irr_meth_fn)//'_'//trim(adjustl(start_year))//'.asc',&
                    & info_spat%irr_meth_id,sim,extent)
            end if
            call read_grid(trim(dir)//trim(sim%eff_net_fn)//'.asc',info_spat%eff_net,sim,extent)
        case (2)
            if (sim%f_soiluse .eqv. .false.) then
                call read_grid(trim(dir)//trim(sim%id_irr_meth_fn)//'.asc',info_spat%irr_meth_id,sim,extent)
                call read_grid(trim(dir)//trim(sim%eff_irr_fn)//'.asc',info_spat%eff_met,sim,extent)
            else
                write(start_year,*) sim%start_year
                call read_grid (trim(dir)//trim(sim%id_irr_meth_fn)//'_'//trim(adjustl(start_year))//'.asc',&
                        & info_spat%irr_meth_id,sim,extent)
                call read_grid (trim(dir)//trim(sim%eff_irr_fn)//'_'//trim(adjustl(start_year))//'.asc',&
                        & info_spat%eff_met,sim,extent)
            end if
            !call read_matrices(trim(dir)//trim(sim%eff_rete_fn)//'.asc',info_spat%eff_rete,sim,confini) - RR
        case (3)
            if (sim%f_soiluse .eqv. .false.) then
                call read_grid(trim(dir)//trim(sim%id_irr_meth_fn)//'.asc',info_spat%irr_meth_id,sim,extent)
            else
                write(start_year,*) sim%start_year
                call read_grid (trim(dir)//trim(sim%id_irr_meth_fn)//'_'//trim(adjustl(start_year))//'.asc',&
                    & info_spat%irr_meth_id,sim,extent)
            end if
            !call read_matrices(trim(dir)//trim(sim%eff_rete_fn)//'.asc',info_spat%eff_rete,sim,confini) - RR
        case (4)
            call read_grid(trim(dir)//trim(sim%irr_units_fn)//'.asc',info_spat%irr_unit_id,sim,extent)
            call overlay_domain(info_spat%irr_unit_id, info_spat%domain, sim%irr_units_fn)
            if (sim%f_soiluse .eqv. .false.) then
                call read_grid(trim(dir)//trim(sim%id_irr_meth_fn)//'.asc',info_spat%irr_meth_id,sim,extent)
                call read_grid(trim(dir)//trim(sim%eff_irr_fn)//'.asc',info_spat%eff_met,sim,extent)
            else
                write(start_year,*) sim%start_year
                call read_grid (trim(dir)//trim(sim%id_irr_meth_fn)//'_'//trim(adjustl(start_year))//'.asc',&
                    & info_spat%irr_meth_id,sim,extent)
                call read_grid (trim(dir)//trim(sim%eff_irr_fn)//'_'//trim(adjustl(start_year))//'.asc',&
                    & info_spat%eff_met,sim,extent)
            end if
            call read_grid(trim(dir)//trim(sim%eff_net_fn)//'.asc',info_spat%eff_net,sim,extent)
        case default
    end select
    print *, "Irrigation distribution parameters have been read"

end subroutine read_irr_grid

subroutine check_grid(info_spat, sim)
    ! check grid extension respect to the domain
    ! TODO: reorganize the function
    type(simulation),intent(inout)::sim
    type(spatial_info),intent(inout)::info_spat
    integer::k
    character(len=30)::k_str

    call overlay_domain(info_spat%theta(1)%sat, info_spat%domain, sim%thetaI_sat_fn )
    call overlay_domain(info_spat%theta(2)%sat, info_spat%domain, sim%thetaII_sat_fn)
    call overlay_domain(info_spat%theta(1)%fc,  info_spat%domain, sim%thetaI_FC_fn  )
    call overlay_domain(info_spat%theta(2)%fc,  info_spat%domain, sim%thetaII_FC_fn )
    call overlay_domain(info_spat%theta(1)%wp,  info_spat%domain, sim%thetaI_WP_fn  )
    call overlay_domain(info_spat%theta(2)%wp,  info_spat%domain, sim%thetaII_WP_fn )
    call overlay_domain(info_spat%theta(1)%r,   info_spat%domain, sim%thetaI_r_fn   )
    call overlay_domain(info_spat%theta(2)%r,   info_spat%domain, sim%thetaII_r_fn  )
    call overlay_domain(info_spat%k_sat(1),     info_spat%domain, sim%ksat_I_fn     )
    call overlay_domain(info_spat%k_sat(2),     info_spat%domain, sim%ksat_II_fn    )
    call overlay_domain(info_spat%fact_n(1),    info_spat%domain, sim%n_I_fn        )
    call overlay_domain(info_spat%fact_n(2),    info_spat%domain, sim%n_II_fn       )
    call overlay_domain(info_spat%slope,        info_spat%domain, sim%slope_fn      )
    call overlay_domain(info_spat%drainage,     info_spat%domain, sim%dren_fn       )
    call overlay_domain(info_spat%hydr_gr,      info_spat%domain, sim%hydr_group_fn )

    if(sim%f_init_wc .eqv. .true.)then  ! Initial moisture condition defined externally
        where(info_spat%domain%mat/=info_spat%domain%header%nan .and. &
                & info_spat%theta(1)%old%mat==info_spat%theta(1)%old%header%nan)
            info_spat%theta(1)%old%mat = info_spat%theta(1)%fc%mat ! use theta_fc to fill the gaps
        end where
        where(info_spat%domain%mat/=info_spat%domain%header%nan .and. &
                & info_spat%theta(2)%old%mat==info_spat%theta(2)%old%header%nan)
            info_spat%theta(2)%old%mat = info_spat%theta(2)%fc%mat
        end where
    else                                ! Initial condition calculated from the first warm-up period
        call overlay_domain(info_spat%theta(1)%old, info_spat%domain, sim%thetaI_FC_fn)
        call overlay_domain(info_spat%theta(2)%old, info_spat%domain, sim%thetaII_FC_fn)
    end if

    ! check the parameters of the capillary rise model
    if(sim%f_cap_rise .eqv. .true.)then
        call overlay_domain(info_spat%wat_tab, info_spat%domain, sim%wat_table_fn)
        if(minval(info_spat%wat_tab%mat,info_spat%wat_tab%mat/=info_spat%wat_tab%header%nan)<0.)then
            print*,"A null value has been set where groundwater levels are negative"
            where(info_spat%wat_tab%mat<0. .and. info_spat%wat_tab%mat/=info_spat%wat_tab%header%nan)info_spat%wat_tab%mat=0.
        end if
        call overlay_domain(info_spat%a3, info_spat%domain, sim%ParRisCap_a3_fn)
        call overlay_domain(info_spat%a4, info_spat%domain, sim%ParRisCap_a4_fn)
        call overlay_domain(info_spat%b1, info_spat%domain, sim%ParRisCap_b1_fn)
        call overlay_domain(info_spat%b2, info_spat%domain, sim%ParRisCap_b2_fn)
        call overlay_domain(info_spat%b3, info_spat%domain, sim%ParRisCap_b3_fn)
        call overlay_domain(info_spat%b4, info_spat%domain, sim%ParRisCap_b4_fn)
        ! Set the minimum value of water table depth
        ! TODO: check what happen if the depth is not set
        ! TODO: check the minimum value of water table
        ! TODO: check when root goes into water table or cut roots
        ! %EAC%: moved in the water balance module
        !where(info_spat%wattab%mat<1.d0 .and. info_spat%wattab%mat/=info_spat%wattab%intest%nan)info_spat%wattab%mat=1.d0
    end if

    ! weather station weights maps
    do k=1,size(info_spat%weight_ws)
        write(k_str,*) k
        call overlay_domain(info_spat%weight_ws(k), info_spat%domain, trim(sim%meteoweight_fn)//" "//trim(adjustl(k_str)))
    end do

    info_spat%backup_domain = info_spat%domain
    info_spat%backup_domain%mat = info_spat%domain%mat

    ! land use
    call overlay_domain(info_spat%soil_use_id, info_spat%domain, sim%soiluse_fn)
    if(minval(info_spat%soil_use_id%mat,info_spat%soil_use_id%mat/=info_spat%soil_use_id%header%nan) < 1 &
            .or. maxval(info_spat%soil_use_id%mat) > sim%n_lus)then
        print *,"Soil use maps have soil uses not defined in crop database"
        print *, 'Verify the landuse list at ', trim(join_path(sim%crop_inputs_path, sim%soil_uses_fn, delimiter))
        print *, 'Execution will be aborted...'
        stop
    end if
end subroutine check_grid

subroutine check_irr_grid(info_spat, sim)
    ! check irrigation parameters grids
    type(simulation),intent(inout)::sim
    type(spatial_info),intent(inout)::info_spat
    integer::ios
    character(len=300)::dir
    integer :: line

    dir = sim%input_path
    line = 0
    ios = 0
    call validate_irr_method_map(info_spat%irr_meth_id, info_spat%domain, sim%n_irr_meth, sim%id_irr_meth_fn)
    select case (sim%mode)
        case (1)
            call overlay_domain(info_spat%irr_unit_id, info_spat%domain, sim%irr_units_fn)
            call set_default_par(info_spat%eff_net,info_spat%domain, 1.0D0)
        case (2)
            !call check_mat_irrigation(info_spat%eff_rete,info_spat%domain, 1.0D0) - %RR%
            call set_default_par(info_spat%eff_met, info_spat%domain, 1.0D0)
        case (3)
            !call check_mat_irrigation(info_spat%eff_rete,info_spat%domain, 1.0D0) - %RR%
        case (4)
            call overlay_domain(info_spat%irr_unit_id, info_spat%domain, sim%irr_units_fn)
            call set_default_par(info_spat%eff_met, info_spat%domain, 1.0D0)
            call set_default_par(info_spat%eff_net,info_spat%domain, 1.0D0)
        case default
    end select
    !%PS%: removed set_default_pars for booster parameters; calc_perc_booster_pars already applies the defaults internally
end subroutine check_irr_grid

subroutine validate_irr_method_map(method_grid, domain_grid, n_methods, map_name)
    ! Verify that the irrigation method map contains only valid method IDs (NaNs are removed from the domain)
    type(grid_i), intent(inout) :: method_grid, domain_grid
    integer, intent(in) :: n_methods
    character(len=*), intent(in) :: map_name

    call overlay_domain(method_grid, domain_grid, map_name)
    if (any(domain_grid%mat /= domain_grid%header%nan .and. (method_grid%mat < 0 .or. method_grid%mat > n_methods))) then
        print *, "Error in irrigation method map '", trim(map_name), "': IDs other than NoData must be between 0 and ", n_methods
        print *, "Execution will be aborted..."
        stop
    end if
end subroutine validate_irr_method_map

subroutine read_rice_parameters(sim, theta2_rice)
    ! read the parameters specific for rice paddy
    type(simulation),intent(inout)::sim
    integer::ios
    type(soil2_rice)::theta2_rice
    character(len=300)::dir
    character(len=300) :: comment,buffer, label
    integer :: p
    integer :: line

    dir = sim%input_path
    line = 0
    ios = 0

    open(newunit=theta2_rice%unit_soil_rice,file=trim(dir)//trim(sim%soil_prop_x_rice_fn),status='old',action='read',iostat=ios)
    if (ios /= 0) then
        !%PS%: now rice soil parameters are optional
        print *, "Warning: Couldn't open the rice soil properties file: ", trim(dir)//trim(sim%soil_prop_x_rice_fn), &
               & ". Rice soil properties will not be modified."
        return
    end if

    do while (ios == 0)
        read (theta2_rice%unit_soil_rice, '(A)', iostat = ios) buffer
        if (ios == 0) then
            line = line +1
            buffer = trim(buffer)
            p = scan(buffer, '#')
            if (p /= 0) then
                comment = buffer(p+1:)
                buffer = buffer(1:p-1)
            end if
            if (buffer /= '') then
                call lower_case(buffer)
                p = scan(buffer, '=')
                label = buffer(1:p-1)
                buffer = buffer(p+1:)
                select case (label)
                    case ('ksat_ii');   read (buffer, *, iostat = ios) theta2_rice%k_sat_2
                    case ('n_ii');      read (buffer, *, iostat = ios) theta2_rice%n_2
                    case ('tetaii_fc', 'thetaii_fc'); read (buffer, *, iostat = ios) theta2_rice%theta2_FC
                    case ('tetaii_r',  'thetaii_r' ); read (buffer, *, iostat = ios) theta2_rice%theta2_R
                    case ('tetaii_sat','thetaii_sat');read (buffer, *, iostat = ios) theta2_rice%theta2_SAT
                    case ('tetaii_wp', 'thetaii_wp'); read (buffer, *, iostat = ios) theta2_rice%theta2_WP
                    case default
                        print *, 'Skipping invalid label <',trim(label),'> at line', line, ' of file: ', &
                            & trim(sim%soil_prop_x_rice_fn), '. Execution will be aborted...'
                        stop
                end select
            end if
        end if
    end do
    close(theta2_rice%unit_soil_rice)
end subroutine read_rice_parameters

subroutine init_cn_table(tab_CN2, tab_CN3)
    ! init the CN table
    ! TODO: replace with the initialization from external file
    real(dp),dimension(:,:,:),intent(out):: tab_CN2, tab_CN3

    tab_CN2 = tabCN
    where(tab_CN2 > 0) ! calculate tab_CN3
        tab_CN3=tab_CN2*exp(0.00673*(100.-tab_CN2))
    end where

end subroutine init_cn_table

subroutine write_init_grids(info_spat,mode,path,sim)
    ! save the grid data for the selected area
    type(spatial_info),intent(in)::info_spat
    integer, intent(in)::mode
    character(len=*),intent(in)::path
    type(simulation),intent(in)::sim

    integer :: k
    character(len=30)::meteo_stringa

    call write_grid(trim(path)//'out_'//trim(sim%domain_fn)//'.asc',info_spat%domain)
    call write_grid(trim(path)//'out_'//trim(sim%thetaI_FC_fn)//'.asc',info_spat%theta(1)%fc)
    call write_grid(trim(path)//'out_'//trim(sim%thetaII_FC_fn)//'.asc',info_spat%theta(2)%fc)
    call write_grid(trim(path)//'out_'//trim(sim%thetaI_WP_fn)//'.asc',info_spat%theta(1)%wp)
    call write_grid(trim(path)//'out_'//trim(sim%thetaII_WP_fn)//'.asc',info_spat%theta(2)%wp)
    call write_grid(trim(path)//'out_'//trim(sim%thetaI_r_fn)//'.asc',info_spat%theta(1)%r)
    call write_grid(trim(path)//'out_'//trim(sim%thetaII_r_fn)//'.asc',info_spat%theta(2)%r)
    call write_grid(trim(path)//'out_'//trim(sim%thetaI_SAT_fn)//'.asc',info_spat%theta(1)%sat)
    call write_grid(trim(path)//'out_'//trim(sim%thetaII_SAT_fn)//'.asc',info_spat%theta(2)%sat)
    call write_grid(trim(path)//'out_'//trim(sim%slope_fn)//'.asc',info_spat%slope)
    call write_grid(trim(path)//'out_'//trim(sim%dren_fn)//'.asc',info_spat%drainage)
    call write_grid(trim(path)//'out_'//trim(sim%hydr_group_fn)//'.asc',info_spat%hydr_gr)
    call write_grid(trim(path)//'out_'//trim(sim%ksat_I_fn)//'.asc',info_spat%k_sat(1))
    call write_grid(trim(path)//'out_'//trim(sim%ksat_II_fn)//'.asc',info_spat%k_sat(2))
    call write_grid(trim(path)//'out_'//trim(sim%n_I_fn)//'.asc',info_spat%fact_n(1))
    call write_grid(trim(path)//'out_'//trim(sim%n_II_fn)//'.asc',info_spat%fact_n(2))
    call write_grid(trim(path)//'out_'//trim(sim%soiluse_fn)//'.asc',info_spat%soil_use_id)
    call write_grid(trim(path)//'out_'//trim(sim%wat_table_fn)//'.asc',info_spat%wat_tab)
    call write_grid(trim(path)//'out_'//trim(sim%ParRisCap_a3_fn)//'.asc',info_spat%a3)
    call write_grid(trim(path)//'out_'//trim(sim%ParRisCap_a4_fn)//'.asc',info_spat%a4)
    call write_grid(trim(path)//'out_'//trim(sim%ParRisCap_b1_fn)//'.asc',info_spat%b1)
    call write_grid(trim(path)//'out_'//trim(sim%ParRisCap_b2_fn)//'.asc',info_spat%b2)
    call write_grid(trim(path)//'out_'//trim(sim%ParRisCap_b3_fn)//'.asc',info_spat%b3)
    call write_grid(trim(path)//'out_'//trim(sim%ParRisCap_b4_fn)//'.asc',info_spat%b4)

    select case (mode)
        case (1)
            call write_grid(trim(path)//'out_'//trim(sim%irr_units_fn)//'.asc',info_spat%irr_unit_id)
            call write_grid(trim(path)//'out_'//trim(sim%eff_net_fn)//'.asc',info_spat%eff_net)
            call write_grid(trim(path)//'out_'//trim(sim%id_irr_meth_fn)//'.asc',info_spat%irr_meth_id)
            call write_grid(trim(path)//"out_am_percI.asc",info_spat%a_perc(1))
            call write_grid(trim(path)//"out_am_percII.asc",info_spat%a_perc(2))
            call write_grid(trim(path)//"out_bm_percI.asc",info_spat%b_perc(1))
            call write_grid(trim(path)//"out_bm_percII.asc",info_spat%b_perc(2))
        case (2)
            call write_grid(trim(path)//'out_'//trim(sim%eff_irr_fn)//'.asc',info_spat%eff_met)
            !call write_matrices(trim(path)//'out_'//trim(sim%eff_rete_fn)//'.asc',info_spat%eff_rete)! - RR
            call write_grid(trim(path)//'out_'//trim(sim%id_irr_meth_fn)//'.asc',info_spat%irr_meth_id)
            call write_grid(trim(path)//"out_am_percI.asc",info_spat%a_perc(1))
            call write_grid(trim(path)//"out_am_percII.asc",info_spat%a_perc(2))
            call write_grid(trim(path)//"out_bm_percI.asc",info_spat%b_perc(1))
            call write_grid(trim(path)//"out_bm_percII.asc",info_spat%b_perc(2))
        case (3)
            !call write_matrices(trim(path)//'out_'//trim(sim%eff_rete_fn)//'.asc',info_spat%eff_rete)! - RR
            call write_grid(trim(path)//'out_'//trim(sim%id_irr_meth_fn)//'.asc',info_spat%irr_meth_id)
            call write_grid(trim(path)//"out_am_percI.asc",info_spat%a_perc(1))
            call write_grid(trim(path)//"out_am_percII.asc",info_spat%a_perc(2))
            call write_grid(trim(path)//"out_bm_percI.asc",info_spat%b_perc(1))
            call write_grid(trim(path)//"out_bm_percII.asc",info_spat%b_perc(2))
        case (4)
            call write_grid(trim(path)//'out_'//trim(sim%irr_units_fn)//'.asc',info_spat%irr_unit_id)
            call write_grid(trim(path)//'out_'//trim(sim%eff_irr_fn)//'.asc',info_spat%eff_met)
            call write_grid(trim(path)//'out_'//trim(sim%eff_net_fn)//'.asc',info_spat%eff_net)
            call write_grid(trim(path)//'out_'//trim(sim%id_irr_meth_fn)//'.asc',info_spat%irr_meth_id)
            call write_grid(trim(path)//"out_am_percI.asc",info_spat%a_perc(1))
            call write_grid(trim(path)//"out_am_percII.asc",info_spat%a_perc(2))
            call write_grid(trim(path)//"out_bm_percI.asc",info_spat%b_perc(1))
            call write_grid(trim(path)//"out_bm_percII.asc",info_spat%b_perc(2))
        case default
    end select

    !meteo weigths
    do k=1,size(info_spat%weight_ws)
        write(meteo_stringa,*) k
        call write_grid(trim(path)//trim(adjustl('out_meteo_'//trim(adjustl(meteo_stringa))//'.asc')), info_spat%weight_ws(k))
    end do

end subroutine write_init_grids

subroutine init_irrigation_units(domain_map,irr_units_map,eff_net,irr_units_tbl,wat_src_tbl,pars,h_met)
    !allocazione e inizializzazione della variabile IU
    type(grid_i),intent(in)::domain_map,irr_units_map
    type(grid_r),intent(in)::eff_net,h_met
    type(parameters),intent(in)::pars
    type(irr_units_table),dimension(:),allocatable,intent(out)::irr_units_tbl
    type(water_sources_table),dimension(:),intent(inout)::wat_src_tbl

    integer::i,free_unit,ios
    integer:: strlen
    character(len=999) :: str                            ! File string
    character(len=1),parameter :: tab = achar(9)   ! Delimiter: horizontal tab
    integer::cols, n_irr_units                                 ! File columns (cols) and rows (n_bac = irrigation district number)

    strlen = 999
    ! Read "<irrdistr_list>.txt"
    open(newunit=free_unit, file=trim(pars%sim%watsour_path)//trim(pars%sim%irrdistr_list_fn), action="read", iostat=ios)
    if(ios/=0)then
        print *, "Error opening file ", trim(pars%sim%irrdistr_list_fn), &
            & "; iostat=", ios, '. Execution will be aborted...'
        stop
    end if

    read(free_unit,'(a)') str       ! Read file header (considering also delimiters and spaces: it needs formatting)
    strlen = len(trim(str))

    cols = 1                        ! Count the number of delimiters in the first line
    do i = 1, strlen
        if (str(i:i) == tab) then
            cols = cols+1
        end if
    end do
    if (cols /= 3) then
        print *, 'Error in file formatting: ', trim(pars%sim%irrdistr_list_fn), ' has ', cols, &
            & ' columns instead of 3. Execution will be aborted...'
        stop
        !!! TODO: %AB%: add wat_shift reading
    end if

    ! Count the number of rows
    n_irr_units = 0
    do
        read (free_unit,*,iostat=ios) str
        if (ios/=0) exit
        n_irr_units = n_irr_units + 1
    end do

    allocate(irr_units_tbl(n_irr_units))          ! Allocate variable with irrigation district number

    rewind(free_unit)                ! Rewind of file to store its content
    read(free_unit,*) str            ! Skip header line

    !%PS%, todo: decide whether n_cells/int_distr_eff/h_irr_mean should include non-irrigable cells (e.g. who have irr_method=0 or whose crop have irrigation_class=0)
    do i = 1, n_irr_units
!~             read(free_unit,*,iostat=ios)IU(i)%id,IU(i)%f_explore,IU(i)%f_un_priv,IU(i)%wat_shift
        read(free_unit,*,iostat=ios)irr_units_tbl(i)%id,irr_units_tbl(i)%f_explore,irr_units_tbl(i)%f_un_priv
        irr_units_tbl(i)%wat_shift = 1      ! set wat_shift to 1 by default

        ! Calculate the number of irrigable cells in a day by counting the cells belonging to each irrigation units
        ! The value is yearly updated because landuse can change
        ! The value if rounded to the higher integer
        irr_units_tbl(i)%n_cells=count(irr_units_map%mat==irr_units_tbl(i)%id .and. domain_map%mat/=domain_map%header%nan)

        ! Calculate the maximum number of cells processed in a day
        irr_units_tbl(i)%n_day= ceiling (irr_units_tbl(i)%n_cells / irr_units_tbl(i)%wat_shift)
        ! set n_day max to n_cells
        if (irr_units_tbl(i)%n_day > irr_units_tbl(i)%n_cells) irr_units_tbl(i)%n_day = irr_units_tbl(i)%n_cells

        if (irr_units_tbl(i)%n_cells > 0) then
            ! calculate the average network efficiency: SUM(net_eff)/ n_cells
            irr_units_tbl(i)%int_distr_eff = sum(eff_net%mat, irr_units_map%mat==irr_units_tbl(i)%id .and. &
                                               & domain_map%mat/=domain_map%header%nan) / irr_units_tbl(i)%n_cells
            ! calculate the average irrigation height
            irr_units_tbl(i)%h_irr_mean = sum(h_met%mat, irr_units_map%mat==irr_units_tbl(i)%id .and. &
                                            & domain_map%mat/=domain_map%header%nan) / irr_units_tbl(i)%n_cells
        else
            ! This IU is listed in irr_districts.txt but has no cells in the active domain. Set values to 0 avoiding unsafe math
            irr_units_tbl(i)%int_distr_eff = 0.0_dp
            irr_units_tbl(i)%h_irr_mean = 0.0_dp
        end if
        if(ios/=0)then
            print *, 'Error reading file ', trim(pars%sim%irrdistr_list_fn), ' in line ', i+1, '.'
            print *, 'Expected format: integer real integer. Execution will be aborted...'
            stop
        end if
    end do
    close(free_unit)

    ! init the reference to the water sources table
    do i=1,size(wat_src_tbl)
        wat_src_tbl(i)%irr_unit_idx = get_value_index(irr_units_tbl%id,wat_src_tbl(i)%id_irr_unit)
    end do

    ! init the discharges to zeros
    irr_units_tbl%q_day=0.;irr_units_tbl%q_nom=0.
    irr_units_tbl%q_act_fld(1)=0.;irr_units_tbl%q_act_fld(2)=0.;irr_units_tbl%q_act_fld(3)=0.;irr_units_tbl%q_act_fld(4)=0.
    irr_units_tbl%q_pot_fld(1)=0.;irr_units_tbl%q_pot_fld(2)=0.;irr_units_tbl%q_pot_fld(3)=0.;irr_units_tbl%q_pot_fld(4)=0.
    irr_units_tbl%n_irrigated_cells=0
    irr_units_tbl%q_un_priv=0

    if (pars%sim%prt_debug_out=='y') then
        open(newunit=free_unit, file=(trim(pars%sim%path)//"out_"//trim(pars%sim%watsources_fn)), action="write")
        write(free_unit,*)'distr_id; source_code; source_type; flow_ratio; distr_column; watsour_column'
        do i=1,size(wat_src_tbl)
            write(free_unit,*)wat_src_tbl(i)%id_irr_unit,'; ',wat_src_tbl(i)%id_wat_src,'; ',wat_src_tbl(i)%type_id, &
                & '; ',wat_src_tbl(i)%duty_frc,'; ',wat_src_tbl(i)%irr_unit_idx,'; ',wat_src_tbl(i)%wat_src_idx
        end do
        close(free_unit)

        !%AB%: add duration
        call init_cell_output_file(free_unit,trim(pars%sim%path)//"irrdistricts_log.csv",&
            & 'distr_id; cells_no; wat_shift; cell_day; conv_eff; havmet; privatewells')
        do i=1,n_irr_units
            write(free_unit,*)irr_units_tbl(i)%id,'; ',irr_units_tbl(i)%n_cells,&
                & '; ',irr_units_tbl(i)%wat_shift,'; ',irr_units_tbl(i)%n_day,'; ',&
                & '; ',irr_units_tbl(i)%int_distr_eff,'; ',&
                & irr_units_tbl(i)%h_irr_mean,'; ',irr_units_tbl(i)%f_un_priv
        end do
        close(free_unit)
    end if

end subroutine init_irrigation_units

subroutine print_all(flag, sim)
    character, intent(in) :: flag
    type(simulation), intent(inout) :: sim

    ! assign the same value to all the prt conditions
    sim%prt_stp_rain = flag
    sim%prt_stp_transp_act = flag
    sim%prt_stp_transp_pot = flag
    sim%prt_stp_irr = flag
    sim%prt_stp_irr_loss = flag
    sim%prt_stp_cap_rise = flag
    sim%prt_stp_irr_nm_priv = flag
    sim%prt_stp_irr_nm_col = flag
    sim%prt_stp_deep_perc = flag
    sim%prt_stp_runoff = flag
    sim%prt_stp_et_pot = flag
    sim%prt_stp_et_act = flag
    sim%prt_dbg_eva_act = flag
    sim%prt_dbg_eff_rain = flag
    sim%prt_dbg_perc1 = flag
    sim%prt_dbg_perc2 = flag
    sim%prt_dbg_h_soil1 = flag
    sim%prt_dbg_h_soil2 = flag
    sim%prt_yr_rain = flag
    sim%prt_yr_rain_crop_season = flag
    sim%prt_yr_irr = flag
    sim%prt_yr_irr_loss = flag
    sim%prt_yr_eva_act_crop_season = flag
    sim%prt_yr_eva_pot_crop_season = flag
    sim%prt_yr_transp_act = flag
    sim%prt_yr_transp_pot = flag
    sim%prt_yr_runoff = flag
    sim%prt_yr_net_flux_gw = flag
    sim%prt_yr_total_eff = flag
    sim%prt_yr_n_irr_events = flag
    sim%prt_yr_h_irr_mean = flag
    sim%prt_yr_biomass_pot = flag
    sim%prt_yr_yield_pot = flag
    sim%prt_yr_yield_act = flag
    sim%prt_yr_T_act_sum = flag
    sim%prt_yr_T_pot_sum = flag
    sim%prt_yr_f_WS_stage = flag
    sim%prt_yr_f_WS = flag
    sim%prt_yr_f_HS = flag
    sim%prt_yr_f_HS_sum = flag
    sim%prt_yr_dbg_eva_act_tot = flag
    sim%prt_yr_dbg_rain_eff = flag
    sim%prt_yr_dbg_iter1 = flag
    sim%prt_yr_dbg_iter2 = flag
    ! cell outputs
    sim%prt_cell_convergence = flag
    sim%prt_cell_evaporation = flag
    sim%prt_cell_runoff = flag
    sim%prt_cell_et0 = flag
    ! not classified debug outputs
    sim%prt_debug_out = flag

    ! initial conditions
    sim%prt_init_cond = flag
end subroutine

subroutine print_step(flag, sim)
    character, intent(in) :: flag
    type(simulation), intent(inout) :: sim

    ! assign the same value to all the prt conditions
    sim%prt_stp_rain = flag
    sim%prt_stp_transp_act = flag
    sim%prt_stp_transp_pot = flag
    sim%prt_stp_irr = flag
    sim%prt_stp_irr_loss = flag
    sim%prt_stp_cap_rise = flag
    sim%prt_stp_irr_nm_priv = flag
    sim%prt_stp_irr_nm_col = flag
    sim%prt_stp_deep_perc = flag
    sim%prt_stp_runoff = flag
    sim%prt_stp_et_pot = flag
    sim%prt_stp_et_act = flag
end subroutine

subroutine print_annual(flag, sim)
    character, intent(in) :: flag
    type(simulation), intent(inout) :: sim

    ! assign the same value to all the prt conditions
    sim%prt_yr_rain = flag
    sim%prt_yr_rain_crop_season = flag
    sim%prt_yr_irr = flag
    sim%prt_yr_irr_loss = flag
    sim%prt_yr_eva_act_crop_season = flag
    sim%prt_yr_eva_pot_crop_season = flag
    sim%prt_yr_transp_act = flag
    sim%prt_yr_transp_pot = flag
    sim%prt_yr_runoff = flag
    sim%prt_yr_net_flux_gw = flag
    sim%prt_yr_total_eff = flag
    sim%prt_yr_n_irr_events = flag
    sim%prt_yr_h_irr_mean = flag
end subroutine

subroutine print_yield(flag, sim)
    character, intent(in) :: flag
    type(simulation), intent(inout) :: sim

    ! assign the same value to all the prt conditions
    sim%prt_yr_biomass_pot = flag
    sim%prt_yr_yield_pot = flag
    sim%prt_yr_yield_act = flag
    sim%prt_yr_T_act_sum = flag
    sim%prt_yr_T_pot_sum = flag
    sim%prt_yr_f_WS_stage = flag
    sim%prt_yr_f_WS = flag
    sim%prt_yr_f_HS = flag
    sim%prt_yr_f_HS_sum = flag

end subroutine

subroutine print_debug(flag, sim)
    character, intent(in) :: flag
    type(simulation), intent(inout) :: sim

    ! assign the same value to all the prt conditions
    sim%prt_dbg_eva_act = flag
    sim%prt_dbg_eff_rain = flag
    sim%prt_dbg_perc1 = flag
    sim%prt_dbg_perc2 = flag
    sim%prt_dbg_h_soil1 = flag
    sim%prt_dbg_h_soil2 = flag
    sim%prt_yr_dbg_eva_act_tot = flag
    sim%prt_yr_dbg_rain_eff = flag
    sim%prt_yr_dbg_iter1 = flag
    sim%prt_yr_dbg_iter2 = flag

    ! cell output
    sim%prt_cell_convergence = flag
    sim%prt_cell_evaporation = flag
    sim%prt_cell_runoff = flag
    sim%prt_cell_et0 = flag

    ! not classified debug outputs
    sim%prt_debug_out = flag

    ! initial conditions
    sim%prt_init_cond = flag

end subroutine

subroutine print_cell_all(flag, sim)
    character, intent(in) :: flag
    type(simulation), intent(inout) :: sim

    ! cell output
    sim%prt_cell_convergence = flag
    sim%prt_cell_evaporation = flag
    sim%prt_cell_runoff = flag
    sim%prt_cell_et0 = flag
end subroutine


end module
