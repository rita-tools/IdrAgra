module cli_simulation_manager
use mod_date, only: date, month_lengths, days_in_year, days_between_dates, advance_calendar_date, day_of_week
use mod_utility, only: dp, get_value_index, get_uniform_sample, itoa, join_path
use mod_parameters
use mod_grid, only: grid_i, read_grid, write_grid, print_mat_as_grid, overlay_domain, bound, id_to_par, set_default_par
use mod_evapotranspiration, only: ET_reference, calculateDLH
use mod_meteo, only: meteo_info, meteo_mat, read_meteo_data, create_meteo_matrices, skip_meteo_days
use mod_runoff
use mod_crop_soil_water
use mod_crop_phenology, only: crop_definition, crop_rotation, crop_pars_matrices, calculate_RF_t
use mod_cropcoef, only: advance_crops_daily, crop_weather_series, load_crop_weather
use mod_constants, only: tmax_time, tmin_time, pi, cost_fwEva, nan_i, nan_r
use mod_common, only: wat_matrix, soil2_rice, hourly
use mod_irrigation
use mod_coupling, only: mf_coupling_state, init_mf_coupling, accumulate_mf_fluxes, coupling_is_due, coupling_handshake
use cli_watsources
use cli_save_outputs
use mod_crop_yield, only: yield_t, yield_accumulator, initialize_yield, destroy_yield, initialize_yield_accumulator, &
                        & start_yield_for_new_crops, accumulate_daily_yield, finalize_harvested_yield
use cli_read_parameter
implicit none

interface assignment(=) !eq_extensive
    module procedure eq_extensive
end interface

type(mf_coupling_state), save :: coupling

contains

subroutine simulation_manager(pars, info_spat, wat_src_tbl, info_sources, info_meteo, crops, bare_soil,      &
                            & crop_rotations, kcb_corr_fact, crop_state, crop_yield_state, tab_CN2, tab_CN3, &
                            & theta2_rice, simulation_end, boundaries, debug, summary, is_warmup             )

    type(parameters),intent(inout)::pars
    real(dp),dimension(:,:,:),intent(in):: tab_CN2, tab_CN3
    type(date), intent(in) :: simulation_end
    logical, intent(in) :: is_warmup
    type(bound),intent(in)::boundaries
    logical,intent(in)::debug,summary
    type(soil2_rice),intent(in)::theta2_rice
    type(spatial_info),intent(inout)::info_spat
    type(water_sources_table),dimension(:),intent(inout)::wat_src_tbl
    type(source_info),intent(inout)::info_sources
    type(meteo_info),dimension(:),intent(inout)::info_meteo
    type(crop_definition), dimension(:), intent(in) :: crops
    type(crop_definition), intent(in) :: bare_soil
    type(crop_rotation), dimension(:), intent(inout) :: crop_rotations
    real(dp), dimension(:,:), intent(in) :: kcb_corr_fact
    type(crop_pars_matrices), intent(inout) :: crop_state
    type(yield_accumulator), intent(inout) :: crop_yield_state

    type(balance_matrices) :: bal, bal_old
    type(crop_pars_matrices)::pheno
    type(grid_i) :: crop_map
    type(crop_weather_series), allocatable :: crop_weather(:)
    type(meteo_mat)::meteo
    real(dp),dimension(info_spat%domain%header%imax,info_spat%domain%header%jmax)::out_cn_day
    type(output_CN),dimension(info_spat%domain%header%imax,info_spat%domain%header%jmax)::out_cn
    type(hourly)::wat_bal_hour
    type(wat_matrix)::wat
    type(output_table_list)::out_tbl_list
    type(step_map)::stp_map
    type(step_debug_map)::deb_map
    type(annual_map)::yr_map
    type(annual_debug_map)::yr_deb_map
    type(yield_t)::yield
    type(irr_units_table),dimension(:),allocatable::irr_units      ! Allocated in mod_watsources
    type(scheduled_irrigation),dimension(:),allocatable::irr_sch ! Allocated in 'open_scheduled_irrigation' function

    integer :: i, j, k, y, doy, hour, z ! for cycles
    integer,dimension(info_spat%domain%header%imax,info_spat%domain%header%jmax,size(info_spat%weight_ws))::dir_meteo
    real(dp),dimension(info_spat%domain%header%imax,info_spat%domain%header%jmax,size(info_spat%weight_ws))::meteo_weight
    real(dp),dimension(info_spat%domain%header%imax,info_spat%domain%header%jmax,pars%sim%n_irr_meth)::h_irr ! z depends on number of irrigation methods
    real(dp),dimension(info_spat%domain%header%imax,info_spat%domain%header%jmax)::priv_irr
    real(dp),dimension(info_spat%domain%header%imax,info_spat%domain%header%jmax)::coll_irr
    integer, dimension(:), allocatable :: out_steps
    integer :: days_before_1st_interval, first_simulated_doy, last_simulated_doy, n_simulation_years
    character(len=5) :: step_label
    integer::xx,yy ! Test cells coordinates
    integer,dimension(info_spat%domain%header%imax,info_spat%domain%header%jmax)::iter1,iter2
    real(dp),dimension(info_spat%domain%header%imax,info_spat%domain%header%jmax)::irr_loss ! Irrigation application losses
    real(dp),dimension(info_spat%domain%header%imax,info_spat%domain%header%jmax)::alpha_ms_map, alpha_unm_map
    real(dp),dimension(info_spat%domain%header%imax,info_spat%domain%header%jmax)::fw_irr
    real(dp),dimension(info_spat%domain%header%imax,info_spat%domain%header%jmax)::fw_day, fw_old! fw daily updated
    real(dp),dimension(info_spat%domain%header%imax,info_spat%domain%header%jmax)::fc ! cover fraction - %RR%
    real(dp),dimension(info_spat%domain%header%imax,info_spat%domain%header%jmax)::a_loss, b_loss, c_loss, f_interception ! application losses model
    real(dp),dimension(info_spat%domain%header%imax,info_spat%domain%header%jmax)::h_irr_sum, h_bypass, h_met_use
    real(dp),dimension(info_spat%domain%header%imax,info_spat%domain%header%jmax)::k_sat2_use, fact_n2_use
    logical, dimension(info_spat%domain%header%imax,info_spat%domain%header%jmax) :: is_rice_paddy
    integer :: dos, year_idx, simulation_year_idx, days_before_simulation
    integer,dimension(info_spat%domain%header%imax,info_spat%domain%header%jmax)::day_from_irr ! days past from the latest irrigation event
    real(dp),dimension(info_spat%domain%header%imax,info_spat%domain%header%jmax,2)::esp_perc  ! exponent of the percolation model
    type(date) :: current ! Current date
    integer :: tmax_d, tmin_d, hr, lat_num
    real(dp):: h_irr_hour, lat_sum, lat_mean, DLH
    type(grid_r)::pheno_grd

    pheno_grd = info_spat%domain
    crop_map = info_spat%domain; crop_map%mat = 0

    ! init irrigation time limits
    info_spat%irr_starts = info_spat%domain
    info_spat%irr_ends = info_spat%domain
    info_spat%irr_starts%mat = pars%sim%start_irr_season
    info_spat%irr_ends%mat = pars%sim%end_irr_season

    ! init maximum pond
    info_spat%h_maxpond=info_spat%slope
    info_spat%h_maxpond%mat = pars%sim%h_maxpond!10000.0D0

    ! spread irrigation variables
    if (pars%sim%mode>0) then
        info_spat%irr_ends%mat=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%irr_ends)
        info_spat%irr_starts%mat=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%irr_starts)
    end if

    ! init to zero irrigation related outputs
    irr_loss = 0.0D0

    ! Variables allocation
    call allocate_all(stp_map, yr_map, deb_map, yr_deb_map, bal, bal_old, wat_bal_hour, meteo, wat, pheno,   &
                    & info_spat%domain%header%imax, info_spat%domain%header%jmax, info_spat%domain%mat, crops)

    if (allocated(crop_state%crop_id)) pheno = crop_state
    call initialize_yield_accumulator(crop_yield_state, info_spat%domain%mat)

    ! Load temperatures time series at the station level and save them in crop_weather (they're needed for phenology calculations)
    if (is_warmup) then
        call load_crop_weather(crop_weather, info_meteo, pars%sim%meteo_path, pars%sim%start, simulation_end)
    else
        call load_crop_weather(crop_weather, info_meteo, pars%sim%meteo_path)
    end if

    ! Make sure that RF-related variables are nan outside the simulation domain
    pheno%d_t_max  = dble(info_spat%domain%header%nan)
    pheno%RF_t_max = dble(info_spat%domain%header%nan)
    pheno%RF_t     = dble(info_spat%domain%header%nan)
    pheno%RF_e     = dble(info_spat%domain%header%nan)

    ! dir_meteo: for each cell, the appropriate meteorological stations are selected
    ! (by changing meteorological station ID to its progressive number in meteorological stations list)
    do k=1,size(dir_meteo,3)
        dir_meteo(:,:,k) = int(info_spat%weight_ws(k)%mat(:,:))
        do j=1,size(info_spat%domain%mat,2)
            do i=1,size(info_spat%domain%mat,1)
                if(info_spat%backup_domain%mat(i,j)/=info_spat%backup_domain%header%nan) then !%PS% changed from %domain to %backup_domain to avoid problems with cells that are not simulated in the first year but become part of the active domain later
                    dir_meteo(i,j,k) = get_value_index(info_meteo%station_id, dir_meteo(i,j,k))
                    if (dir_meteo(i,j,k)==0) then
                        print*,"The weather station with ID", int(info_spat%weight_ws(k)%mat(i,j)), &
                             & "cannot be found in the meteorological stations list. Execution will be aborted..."
                        stop
                    end if
                    meteo_weight(i,j,k) = info_spat%weight_ws(k)%mat(i,j) - int(info_spat%weight_ws(k)%mat(i,j))
                end if
            end do
        end do
    end do

    fw_day = cost_fwEva ! Initialization of fw, set to 1
    fw_old = fw_day
    select case (pars%sim%mode)
        case (0)
            info_spat%irr_meth_id=info_spat%domain
            info_spat%irr_meth_id%mat=1                      ! Methods matrix set to 1
            info_spat%h_meth=info_spat%domain
            info_spat%h_meth%mat=0                           ! Qwat == 0 for each irrigation method

            !%PS% safe initialization values for variables that are used even in mode 0
            alpha_ms_map = 1.0D0
            alpha_unm_map = 1.0D0
            fw_irr = 1.0D0

            f_interception=info_spat%domain%mat
            f_interception=1                                                   ! Flag interception set to 1
        case (1)
            info_spat%h_meth=info_spat%domain
            info_spat%h_meth%mat=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%h_irr)  ! Spreads irrigation height for each irrigation method
            call init_irrigation_units(info_spat%domain,info_spat%irr_unit_id,info_spat%eff_net,irr_units,wat_src_tbl,&
                &pars,info_spat%h_meth)
            alpha_ms_map=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%irr_th_ms)  ! Spreads irrigation threshold for each irrigation method
            alpha_unm_map=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%irr_th_unm)! Spreads irrigation threshold for each irrigation method
            fw_irr=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%f_wet)     ! Spreads wetted fraction for each irrigation method
            a_loss=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%a_loss)    ! Spreads irrigation application loss pars for each irrigation method
            b_loss=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%b_loss)
            c_loss=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%c_loss)
            f_interception=id_to_par(info_spat%irr_meth_id,dble(pars%irr%met(:)%f_interception))
        case (2)
            alpha_ms_map=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%irr_th_ms)
            alpha_unm_map=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%irr_th_unm)
            fw_irr=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%f_wet)
            f_interception=id_to_par(info_spat%irr_meth_id,dble(pars%irr%met(:)%f_interception))
        case (3)
            info_spat%h_meth=info_spat%domain
            info_spat%h_meth%mat=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%h_irr)
            alpha_ms_map=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%irr_th_ms)
            alpha_unm_map=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%irr_th_unm)
            fw_irr=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%f_wet)
            a_loss=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%a_loss)
            b_loss=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%b_loss)
            c_loss=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%c_loss)
            f_interception=id_to_par(info_spat%irr_meth_id,dble(pars%irr%met(:)%f_interception))
        case (4)
            info_spat%h_meth=info_spat%domain
            info_spat%h_meth%mat=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%h_irr)
            alpha_ms_map=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%irr_th_ms)
            alpha_unm_map=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%irr_th_unm)
            fw_irr=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%f_wet)
            a_loss=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%a_loss)
            b_loss=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%b_loss)
            c_loss=id_to_par(info_spat%irr_meth_id,pars%irr%met(:)%c_loss)
            f_interception=id_to_par(info_spat%irr_meth_id,dble(pars%irr%met(:)%f_interception))
        case default
    end select

    dos = 0
    current = pars%sim%start ! Initialize current date to simulation start date

    ! Skip leading info_meteo days
    days_before_simulation = days_between_dates(info_meteo(1)%start, pars%sim%start)
    if (days_before_simulation > 0) call skip_meteo_days(info_meteo, days_before_simulation)

    n_simulation_years = simulation_end%year - pars%sim%start%year + 1

    ! Usually initialized during warmup; retain state for the subsequent production run.
    if (pars%sim%use_modflow_coupling .and. .not. coupling%enabled) then
        call init_mf_coupling(coupling, info_spat%domain)
    end if

    year_cycle: do simulation_year_idx = 1, n_simulation_years

        y = pars%sim%start%year + simulation_year_idx - 1
        year_idx = y - pars%sim%start_year + 1
        first_simulated_doy = merge(pars%sim%start%doy, 1, y == pars%sim%start%year)
        last_simulated_doy = merge(simulation_end%doy, days_in_year(y), y == simulation_end%year)

        ! Read irandom, landuse and irrigation method maps for the new period if needed
        call update_yearly_spatial_data(pars, info_spat, wat_src_tbl, irr_units, boundaries, y,                    &
                                      & alpha_ms_map, alpha_unm_map, fw_irr, a_loss, b_loss, c_loss, f_interception)

        ! Initialize crop calendar
        call initialize_yearly_crop_state(pars, y, info_spat, crop_rotations, yield)

        if (pars%sim%mode == 1) then ! USE mode
                ! Read water sources and dynamic allocation of info_sources%deriv%qt(:,:)
                call read_water_sources(days_in_year(y), pars, info_sources)
                call nom_water_supply(trim(pars%sim%watsour_path)//trim(pars%sim%watsources_fn), &
                    & irr_units, info_sources, wat_src_tbl, pars%sim%f_shapearea, info_spat%domain%header%cellsize, &
                    & info_spat%cell_area%mat, info_spat%irr_unit_id%mat, debug)
        else if (pars%sim%mode == 4) then ! CALENDAR mode
                ! Calculating water supply on the basis of irrigation application calendar
                call open_scheduled_irrigation(trim(pars%sim%watsour_path)//trim(pars%sim%sched_irr_fn), irr_sch, debug)
        end if

        call initialize_yearly_outputs(pars, y, info_meteo, info_spat,                   &
                                     & irr_units, out_tbl_list, yr_map, yield, yr_deb_map)

        !%PS%: Divide the yearly period into output-accumulation intervals according to output time-step
        select case (pars%sim%step_out)
            case (output_monthly)
                out_steps = month_lengths(y)
                days_before_1st_interval = 0
                step_label = 'month'
            case (output_weekly)
                out_steps = weekly_interval_days(pars%sim%weekly_output_weekday, y)
                days_before_1st_interval = 0
                step_label = 'week'
            case (output_periodic)
                out_steps = periodic_interval_days(pars%sim%out_step_start, pars%sim%out_step_end, pars%sim%out_step_days)
                days_before_1st_interval = pars%sim%out_step_start - 1
                step_label = 'step'
            case default
        end select

        ! Daily simulation cycle
        day_cycle: do doy = first_simulated_doy, last_simulated_doy
            print*,'Simulation day', achar(9), doy, achar(9), 'year', achar(9), y

            dos = dos + 1 ! Day of Simulation
            coll_irr = 0; priv_irr = 0; iter1 = 0; iter2 = 0; h_irr = 0.

            ! Tries importing updated water table data into info_spat%wtab
            if (pars%sim%f_cap_rise) call update_water_table(pars, info_spat, boundaries, y, doy)

            if (pars%sim%read_crop_maps) then
                call update_crop_map(pars, crop_map, info_spat%domain, boundaries, crops, y, doy)
                ! Finalize yesterday's crop before replacing it
                where (info_spat%domain%mat /= info_spat%domain%header%nan)
                    pheno%harvest_pending = pheno%crop_id > 0 .and. crop_map%mat /= pheno%crop_id
                end where
                call finalize_harvested_yield(crop_yield_state, yield, pheno%harvest_pending,     &
                                            & crops, pars%sim%co2_conc(year_idx), info_spat%domain)
            end if

            call init_step_output_file(stp_map, pars%sim%path, itoa(y), doy, out_steps,                   &
                                     & days_before_1st_interval, first_simulated_doy, step_label, pars%sim)
            call init_step_debug_output_file(deb_map, pars%sim%path, itoa(y), doy, out_steps,                   &
                                           & days_before_1st_interval, first_simulated_doy, step_label, pars%sim)

            ! read weather daily data and calculate ET0 for each weather stations
            call read_meteo_data(info_meteo, doy, pars%sim%res_canopy(year_idx), pars%sim%forecast_day)

            ! spread weather data to the entire domain
            call create_meteo_matrices(info_meteo, dir_meteo, meteo_weight, meteo, info_spat%domain, &
                                     & doy, pars%sim%res_canopy(year_idx), pars%sim                  )

            ! Advance each cell's crop phenology according to today's GDD accumulation and sow/harvest rules
            call advance_crops_daily(pheno, crops, bare_soil, crop_rotations, info_spat%domain,                  &
                                   & info_spat%soil_use_id, pars%sim%read_crop_maps, crop_map,                   &
                                   & info_spat%irandom%mat, meteo, kcb_corr_fact, dir_meteo, meteo_weight,       &
                                   & pars%sim%interpolate_temp, crop_weather, current, pars%sim%aggressive_sowing)

            call start_yield_for_new_crops(crop_yield_state, pheno, info_spat%domain)

            !%PS%: unified flag for flooded rice special behaviour (soil params swap, irrigation)
            is_rice_paddy = info_spat%domain%mat /= info_spat%domain%header%nan .and.         &! Is in the domain
                            pheno%irrigation_class == 1 .and.                                 &! Is an irrigable crop
                            pheno%cn_class == 7 .and.                                         &! Is a CN=7 crop (rice)
                            doy >= info_spat%irr_starts%mat .and. doy <= info_spat%irr_ends%mat! We are in irrigation season

            ! Inizialization on first day of simulation
            first_day: if (dos == 1) then

                if (pars%sim%f_out_cells) call write_cell_prod(out_tbl_list%prod_info, crops,                  &
                                                             & crop_rotations, pars%sim%co2_conc(year_idx),    &
                                                             & info_spat%soil_use_id%mat, info_spat%irandom%mat)

                day_from_irr=-9999
                esp_perc=1.
                bal%lyr(1)%thickness = pars%depth%ze_fix

                where(info_spat%domain%mat /= info_spat%domain%header%nan)
                    ! Layer depths inizialization - to calculate h_soil and t_soil
                    ! Comparing root zone to evaporative layer depth
                    where(pheno%d_r > pars%depth%ze_fix)
                        bal%lyr(2)%thickness = pheno%d_r - pars%depth%ze_fix
                    else where
                        bal%lyr(2)%thickness = pars%depth%zr_fix
                    end where
                    ! ###################################### %PS%, todo: this is the correct way. #######################################
                    ! ## However, using this leads to a spike in early season irrigation events, rising total irrigation amount by 10% ##
                    !wat_bal2%d_t = pars%depth%zr_fix
                    !where(pheno%is_real_crop)
                    !    wat_bal2%d_t = max(0._dp, pheno%d_r - pars%depth%ze_fix)
                    ! ###################################################################################################################

                    ! Soil water content inizialization [mm]
                    bal%lyr(1)%h_soil = info_spat%theta(1)%old%mat*1000._dp*pars%depth%ze_fix
                    bal%lyr(2)%h_soil = info_spat%theta(2)%old%mat*1000._dp*bal_old%lyr(2)%thickness
                    ! Soil water content inizialization [m3/m3]
                    bal%lyr(1)%t_soil = info_spat%theta(1)%old%mat
                    bal%lyr(2)%t_soil = info_spat%theta(2)%old%mat
                end where

                ! calculate average latitude !%PS%, todo: this should probably be calculated once per year because domain can change
                lat_num = count(info_spat%domain%mat /= info_spat%domain%header%nan)
                lat_sum = sum(meteo%lat, mask=info_spat%domain%mat /= info_spat%domain%header%nan)
                lat_mean = lat_sum / real(lat_num, dp)
            end if first_day

            bal_old = bal

            where(info_spat%domain%mat /= info_spat%domain%header%nan)

                ! %EAC%: limit root depth to water table interface
                where (info_spat%wat_tab%mat<pheno%d_r)
                    pheno%d_r = info_spat%wat_tab%mat
                end where

                ! Layer depths update as a function of d_r (phenological parameter - root depth)
                where(pheno%d_r > pars%depth%ze_fix)
                    bal%lyr(2)%thickness = pheno%d_r - pars%depth%ze_fix
                else where
                    bal%lyr(2)%thickness = pars%depth%zr_fix
                end where
                ! ###################################### %PS%, todo: this is the correct way. #######################################
                ! ## However, using this leads to a spike in early season irrigation events, rising total irrigation amount by 10% ##
                !wat_bal2%d_t = pars%depth%zr_fix
                !where(pheno%is_real_crop)
                !    wat_bal2%d_t = max(0._dp, pheno%d_r - pars%depth%ze_fix)
                ! ###################################################################################################################

                ! Distance (mm) between rootzone and water table (influences capillary uptake)
                bal%depth_under_rz = max(0._dp, info_spat%wat_tab%mat - bal%lyr(1)%thickness - bal%lyr(2)%thickness)

                ! Soil water content update
                where(bal%lyr(2)%thickness == bal_old%lyr(2)%thickness)
                    bal_old%lyr(2)%h_soil = bal_old%lyr(2)%t_soil*1000*bal_old%lyr(2)%thickness
                else where(bal%lyr(2)%thickness > bal_old%lyr(2)%thickness)
                    ! Paddy field correction
                    where(is_rice_paddy)
                        bal_old%lyr(2)%h_soil = bal_old%lyr(2)%t_soil*1000*bal_old%lyr(2)%thickness +                    &
                                                theta2_rice%theta2_FC*1000*(bal%lyr(2)%thickness-bal_old%lyr(2)%thickness)
                    else where
                        bal_old%lyr(2)%h_soil = bal_old%lyr(2)%t_soil*1000*bal_old%lyr(2)%thickness +                        &
                                                info_spat%theta(2)%fc%mat*1000*(bal%lyr(2)%thickness-bal_old%lyr(2)%thickness)
                    end where
                else where
                    bal_old%lyr(2)%h_soil = bal_old%lyr(2)%t_soil*1000*bal_old%lyr(2)%thickness -                  &
                                          bal_old%lyr(2)%t_soil*1000*(bal_old%lyr(2)%thickness-bal%lyr(2)%thickness)
                end where

                pheno%p_day = pheno%p + 0.04*(5.-(bal_old%h_eva_pot + bal_old%lyr(1)%h_transp_pot + bal_old%lyr(2)%h_transp_pot))
                ! pheno%pday amendment if pday values are not in their allowed range [0.1 ; 0.8]
                where (pheno%p_day <0.1) pheno%p_day=0.1
                where (pheno%p_day >0.8) pheno%p_day=0.8 ! TODO: larger values will be permitted in order to consider stress irrigation

            end where

            call calculate_RF_t(bal%lyr(2)%thickness, pheno, info_spat%domain)

            ! Soil water thresholds update (wat variable)
            call update_soil_pars(info_spat%domain, info_spat%theta, bal%lyr(1)%thickness, &
                                & bal%lyr(2)%thickness, wat, theta2_rice, is_rice_paddy    )

            ! Irrigation application thresholds update
            call update_irrigation_thresholds(bal, wat, pheno%p_day, pheno%r_stress, alpha_ms_map, &
                                            & alpha_unm_map, info_spat%domain, pars%sim%mode       )

            ! calculate day length
            call calculateDLH(doy, lat_mean, DLH)
            ! calculate radiation distribution along day and update params
            pars%fet0 = pdf_normal(cost_hrs, 12.5D0, DLH/5)

            ! calculate the effective precipitation for the rice (only precipitation is considered)
            bal%h_interc = calc_interception(meteo%p, pheno)
            bal%h_eff_rain = net_precipitation(meteo%p, bal%h_interc)

            ! calculate the temperature stress factor
            tmax_d = tmax_time(current%month)
            tmin_d = tmin_time(current%month)
            do hr = 8, 14
                meteo%T_ave = (meteo%T_max + meteo%T_min)/2 - &
                    & (meteo%T_max - meteo%T_min)/2 * cos(pi*(hr - tmin_d)/(tmax_d - tmin_d)) + meteo%T_ave
            end do
            meteo%T_ave = meteo%T_ave / (14-8+1)

            ! TODO: implement separated subroutine for each simulation mode
            do z=1, pars%sim%n_irr_meth
                where(info_spat%irr_meth_id%mat==z)
                    info_spat%h_maxpond%mat=pars%irr%met(z)%h_maxpond
                end where
            end do

            ! if outside irrigation season restore default, indipendently from methods
            where (doy < info_spat%irr_starts%mat .or. doy > info_spat%irr_ends%mat)
                info_spat%h_maxpond%mat = pars%sim%h_maxpond
            end where

            ! define irrigation height base on irrigation period and specific condiction
            select case (pars%sim%mode)
                case(0) ! NO IRRIGATION mode
                    ! do nothing

                case (1) ! USE mode
                    ! %AB%: init the the cumulative value at the beginning of the season
                    !if (doy==pars%sim%start_irr_season) irr_units(:)%q_surplus = 0
                    ! %EAC%: as the irrigation season can change with the irrigation methods,
                    ! q_surplus is updated at the beginning of the year
                    ! TODO: manage condition when irrigation season is in winter
                    if (doy==1) irr_units(:)%q_rem = 0

                    ! calculate the daily water duty for each irrigation unit, considering the water distribution efficiency
                    call calc_daily_duty(doy, irr_units, info_sources, wat_src_tbl, info_spat%irr_unit_id,           &
                                       & info_spat%domain, pars, pheno%irrigation_class,                             &
                                       & info_spat%irr_meth_id%mat, (bal_old%lyr(1)%h_soil + bal_old%lyr(2)%h_soil), &
                                       & (bal_old%lyr(1)%h_transp_pot + bal_old%lyr(2)%h_transp_pot),                &
                                       & bal%h_raw, (wat%layer(1)%h_fc + wat%layer(2)%h_fc)                          )

                    ! %EAC%: save irrigation units results
                    call save_irr_unit_debug_data(doy, out_tbl_list, irr_units)

                    !%PS%: precompute tentative irrigation depth for rice so that USE mode can treat it as a fixed height
                    !      (whether irrigation can actually be supplied is decided in irrigation_use).
                    h_met_use = info_spat%h_meth%mat
                    where(is_rice_paddy)
                        h_met_use = ((info_spat%h_meth%mat - bal_old%h_pond) +                                                &!<-- reach a pond level of h_meth
                                     (info_spat%theta(1)%sat%mat*bal_old%lyr(1)%thickness*1000.0D0 - bal_old%lyr(1)%h_soil) + &!<-- replenish 1st layer up to saturation
                                     (wat%layer(2)%h_sat - bal_old%lyr(2)%h_soil) +                                           &!<-- replenish 2nd layer up to saturation
                                     (bal_old%h_eva + bal_old%lyr(2)%h_transp_pot))                                            !<-- add expected ET
                    end where
                    call irrigate_rice(h_met_use, pheno, bal%h_eff_rain, theta2_rice%k_sat_2, is_rice_paddy) !<-- add expected percolation and subtract rain

                    call irrigation_use(info_spat%domain, info_spat%irr_unit_id, pheno%irrigation_class, info_spat%irr_meth_id, &
                                      & irr_units, (bal_old%lyr(1)%h_transp_pot+bal_old%lyr(2)%h_transp_pot),                   &
                                      & (bal_old%lyr(1)%h_soil + bal_old%lyr(2)%h_soil),                                        &
                                      & bal%h_irr_thresh, bal%h_raw, bal%h_priv_thresh,                                         &
                                      & h_irr, doy, priv_irr, coll_irr, pars%sim%f_shapearea, info_spat%cell_area%mat,          &
                                      & h_met_use, info_spat%irr_starts%mat, info_spat%irr_ends%mat, pheno%cn_class             )

                    ! %EAC%: save irrigation units results
                    call save_irr_unit_data(doy, out_tbl_list, irr_units, pars%cr%n_withdrawals)

                    ! update irrigation losses
                    call calc_irrigation_losses(a_loss, b_loss, c_loss, meteo%Wind_vel, 0.5*(meteo%T_max+meteo%T_min), irr_loss)
                    ! calculate net irrigation
                    do z=1, pars%sim%n_irr_meth
                        h_irr(:,:,z) = h_irr(:,:,z) * (1.0-irr_loss/100.0)
                    end do

                    ! %AB% init the cumulative value
                    ! %EAC% not sure that q_surplus must be initialized to zero at the beginning and the end of the irrigation period
                    !if (doy==pars%sim%end_irr_season) irr_units(:)%q_surplus = 0

                case (2) ! NEED mode with field capacity target
                    call irrigation_need_fc(info_spat, h_irr, bal, bal_old, pheno, bal%h_eff_rain, &
                                          & theta2_rice%k_sat_2, is_rice_paddy, pars%sim%fc_ratio  )
                    ! if outside the irrigation period, set irrigation height to zero
                    do z=1, pars%sim%n_irr_meth
                        where(doy<info_spat%irr_starts%mat .or. doy>info_spat%irr_ends%mat) h_irr(:,:,z) = 0.
                    end do
                    irr_loss = 0. ! not consider irrigation losses

                case (3) ! NEED mode with fixed volume
                    call irrigation_need_fixed(info_spat, h_irr, bal, bal_old, pheno, bal%h_eff_rain, &
                                             & theta2_rice%k_sat_2, is_rice_paddy, wat%layer(2)%h_sat )
                    ! update irrigation losses
                    call calc_irrigation_losses(a_loss, b_loss, c_loss, meteo%Wind_vel, 0.5*(meteo%T_max+meteo%T_min),irr_loss)
                    ! if outside the irrigation period, set irrigation height to zero
                    ! and calculate net irrigation
                    do z=1, pars%sim%n_irr_meth
                        where(doy<info_spat%irr_starts%mat .or. doy>info_spat%irr_ends%mat) h_irr(:,:,z) = 0.
                        h_irr(:,:,z) = h_irr(:,:,z) * (1.0-irr_loss/100.0)
                    end do

                case (4)! SCHEDULED mode
                    call irrigation_scheduled(info_spat, doy, y, irr_sch, pheno, h_irr, debug, bal, bal_old, a_loss,      &
                                            & b_loss, c_loss, meteo%Wind_vel, 0.5_dp*(meteo%T_max+meteo%T_min), irr_loss, &
                                            & bal%h_eff_rain, theta2_rice%k_sat_2, is_rice_paddy, wat%layer(2)%h_sat      )
                    ! if outside the irrigation period, set irrigation height to zero
                    ! and calculate net irrigation
                    do z=1, pars%sim%n_irr_meth
                        where(doy<info_spat%irr_starts%mat .or. doy>info_spat%irr_ends%mat) h_irr(:,:,z) = 0.
                        h_irr(:,:,z) = h_irr(:,:,z) * (1.0-irr_loss/100.0)
                    end do

                case default
                    print *, "Invalid simulation mode ", pars%sim%mode, ". Simulation mode should be 0, 1, 2, 3, or 4."

            end select

            h_irr_sum = sum(h_irr,dim=3)

            ! update percolation booster
            select case(pars%sim%mode)
                case (1,2,3,4)
                    call update_adj_perco_parameters(info_spat, h_irr_sum, day_from_irr, esp_perc)
                case default
            end select

            ! irrigation losses due to the irrigation method
            h_bypass = h_irr_sum * irr_loss / (100 - irr_loss)
            where (h_irr_sum/=0) yr_map%n_irr_events%mat = yr_map%n_irr_events%mat +1

            ! calculate intercetion according to the Von Hoyningen-Huene and Braden model
            ! consider both precipitation and above canopy irrigation
            bal%h_interc = calc_interception(meteo%p+h_irr_sum * f_interception, pheno)
            bal%h_eff_rain = net_precipitation(meteo%p+h_irr_sum * f_interception, bal%h_interc)

            ! calculate the CN value
            call CN_table(tab_CN2, tab_CN3,info_spat%drainage,pheno%cn_class,pheno%cn_day, &
                & out_cn_day,info_spat%domain,info_spat%hydr_gr, &
                & info_spat%slope%mat, out_cn,info_spat%theta, &
                & bal%lyr(1)%t_soil,bal%lyr(2)%t_soil)

            ! init water balance variables
            call reset_water_balance_variables(bal)

            ! set soil water content of current day to the previous day
            wat_bal_hour%inten%h_soil1 = bal_old%lyr(1)%h_soil
            wat_bal_hour%inten%h_soil2 = bal_old%lyr(2)%h_soil

            ! the gross available water is the sum of precipitation and above canopy irrigation
            bal%h_gross_av_water = meteo%p + h_irr_sum*f_interception

            ! the net available precipitation is the net precipitation + ponding
            bal%h_net_av_water = bal%h_eff_rain
            !wat_bal1%h_net_av_water = wat_bal1%h_eff_rain  + wat_bal1_old%h_pond

            ! calculate the runoff with the CN model
            call CN_runoff(bal%h_gross_av_water, bal%h_net_av_water, &
                & h_irr_sum*(1-f_interception), info_spat%domain, is_rice_paddy, &
                & bal%h_runoff, out_cn_day, pars%sim%lambda_cn)

            ! HOURLY LOOP OF THE SIMULATION
            k_sat2_use = info_spat%k_sat(2)%mat
            fact_n2_use = info_spat%fact_n(2)%mat
            where(is_rice_paddy)
                k_sat2_use = theta2_rice%k_sat_2
                fact_n2_use = theta2_rice%n_2
            end where

            hr_loop: do hour = 1,24
                ! init the hourly water-balance variables to zero
                wat_bal_hour = 0.0D0
                ! distribute precipitation and infiltration uniformly over the day
                where (info_spat%domain%mat /= info_spat%domain%header%nan)
                    wat_bal_hour%esten%h_eff_rain = bal%h_eff_rain / 24._dp
                    wat_bal_hour%esten%h_inf = bal%h_net_av_water / 24._dp
                end where
                ! calculate parameters for evaporation that remain constant during the day
                if(hour==1) then
                    call b1_no_iter_eva(pheno,meteo, pars%sim%h_prec_lim, wat, fw_day, fw_irr, pars%irr%f_w, fc, &
                                        h_irr_sum, f_interception, info_spat%domain, bal_old, fw_old)
                    wat_bal_hour%inten%h_pond0 = 0.
                    !%CG% 2024-03-29 add total ponding to the first hour
                    wat_bal_hour%esten%h_inf = wat_bal_hour%esten%h_inf + bal_old%h_pond
                end if

                ! compute water balance for each cells
                do j=1, size(info_spat%domain%mat,2)
                    do i=1,size(info_spat%domain%mat,1)
                        if(info_spat%domain%mat(i,j)/=info_spat%domain%header%nan)then

                            !%PS%: only calculate irrigation amount if not in mode 0
                            if (pars%sim%mode /= 0 .and. info_spat%irr_meth_id%mat(i,j) > 0) then
                                h_irr_hour = h_irr(i,j, info_spat%irr_meth_id%mat(i,j))                    * & ! Daily amount for this cell
                                           & pars%irr%met(info_spat%irr_meth_id%mat(i,j))%freq(hour)       * & ! Fraction to be applied each hour of activity
                                           & (1-pars%irr%met(info_spat%irr_meth_id%mat(i,j))%f_interception)   ! 1 - intercepted fraction
                            else
                                h_irr_hour = 0
                            end if

                            ! %RR%: add k_r
                            ! water balance for the evaporative layer
                            call water_balance_evap_lay(h_irr_hour, &
                                & wat_bal_hour%inten%h_soil1(i,j), wat_bal_hour%esten%h_inf(i,j), &
                                & wat_bal_hour%esten%h_eva(i,j), wat_bal_hour%esten%h_eva_pot(i,j), &
                                & wat_bal_hour%esten%h_perc1(i,j), wat_bal_hour%inten%h_pond0(i,j), wat_bal_hour%esten%h_pond(i,j), &
                                & wat_bal_hour%esten%h_transp_act1(i,j), wat_bal_hour%esten%h_transp_pot1(i,j), &
                                & pheno%k_cb(i,j), pheno%p_day(i,j),  meteo%et0(i,j)*pars%fet0(hour), wat_bal_hour%esten%k_e(i,j), &
                                & wat_bal_hour%esten%k_r(i,j), wat_bal_hour%inten%k_s_dry(i,j), wat_bal_hour%inten%k_s_sat(i,j),&
                                & wat_bal_hour%inten%k_s(i,j), wat%kc_max(i,j), wat%few(i,j), pheno%RF_e(i,j), &
                                & wat%wat1_rew(i,j), wat%layer(1)%h_sat(i,j), wat%layer(1)%h_fc(i,j), &
                                & wat%layer(1)%h_wp(i,j), wat%layer(1)%h_r(i,j), &
                                & info_spat%k_sat(1)%mat(i,j), info_spat%fact_n(1)%mat(i,j), &
                                & wat_bal_hour%n_iter1(i,j), esp_perc(i,j,1), wat_bal_hour%n_max1(i,j), doy)

                            ! water balance for the transpirative layer
                            if (bal%lyr(2)%thickness(i,j) > 0._dp) then
                                call water_balance_transp_lay(wat_bal_hour%inten%h_soil2(i,j), wat_bal_hour%esten%h_transp_act2(i,j), &
                                    & wat_bal_hour%esten%h_transp_pot2(i,j), wat_bal_hour%esten%h_perc2(i,j), &
                                    & wat_bal_hour%esten%h_perc1(i,j), &
                                    & wat_bal_hour%inten%k_s_dry(i,j),wat_bal_hour%inten%k_s_sat(i,j), wat_bal_hour%inten%k_s(i,j), &
                                    & wat_bal_hour%esten%h_eva_pot(i,j), wat_bal_hour%esten%h_caprise(i,j), &
                                    & wat_bal_hour%esten%h_rise(i,j), &
                                    & pheno%d_r(i,j), bal%lyr(2)%thickness(i,j), pheno%RF_t(i,j), &
                                    & pheno%k_cb(i,j), pheno%p_day(i,j), &
                                    & meteo%et0(i,j)*pars%fet0(hour), wat%layer(2)%h_sat(i,j), &
                                    & wat%layer(2)%h_fc(i,j), wat%layer(2)%h_wp(i,j), wat%layer(2)%h_r(i,j), &
                                    & k_sat2_use(i,j), fact_n2_use(i,j), &
                                    & info_spat%a3%mat(i,j), info_spat%a4%mat(i,j), &
                                    & info_spat%b1%mat(i,j), info_spat%b2%mat(i,j), &
                                    & info_spat%b3%mat(i,j), info_spat%b4%mat(i,j), &
                                    & bal%depth_under_rz(i,j), wat_bal_hour%n_iter2(i,j), &
                                    & esp_perc(i,j,2),pars%sim%f_cap_rise, wat_bal_hour%n_max2(i,j), doy)
                            else
                                !%PS%: No 2nd layer is allowed if root is very shallow; drainage from layer 1 leaves the profile directly
                                wat_bal_hour%inten%h_soil2(i,j) = 0.0D0
                                wat_bal_hour%esten%h_transp_act2(i,j) = 0.0D0
                                wat_bal_hour%esten%h_transp_pot2(i,j) = 0.0D0
                                wat_bal_hour%esten%h_perc2(i,j) = wat_bal_hour%esten%h_perc1(i,j)
                                wat_bal_hour%esten%h_caprise(i,j) = 0.0D0
                                wat_bal_hour%esten%h_rise(i,j) = 0.0D0
                                wat_bal_hour%n_iter2(i,j) = 0
                            end if

                        end if

                    end do
                end do

                ! TODO: %AB% move to subroutine
                ! update the soil water content of the evaporative layer according to the rise from the transpirative layer
                where (wat_bal_hour%esten%h_rise > 0)
                    wat_bal_hour%inten%h_soil1 = wat_bal_hour%inten%h_soil1 + wat_bal_hour%esten%h_rise
                    wat_bal_hour%esten%h_pond = merge (wat_bal_hour%esten%h_pond + (wat_bal_hour%inten%h_soil1 - wat%layer(1)%h_sat), &
                        & wat_bal_hour%esten%h_pond, wat_bal_hour%inten%h_soil1 > wat%layer(1)%h_sat)
                    wat_bal_hour%inten%h_soil1 = merge (wat%layer(1)%h_sat, wat_bal_hour%inten%h_soil1, &
                        & wat_bal_hour%inten%h_soil1 > wat%layer(1)%h_sat)
                end where

                ! update water balance variables
                bal%lyr(1)%h_soil = wat_bal_hour%inten%h_soil1
                bal%h_inf = bal%h_inf + wat_bal_hour%esten%h_inf
                bal%h_eva = bal%h_eva + wat_bal_hour%esten%h_eva
                bal%h_eva_pot = bal%h_eva_pot + wat_bal_hour%esten%h_eva_pot
                bal%lyr(1)%h_transp_act = bal%lyr(1)%h_transp_act + wat_bal_hour%esten%h_transp_act1
                bal%lyr(1)%h_transp_pot = bal%lyr(1)%h_transp_pot + wat_bal_hour%esten%h_transp_pot1
                bal%lyr(1)%h_perc = bal%lyr(1)%h_perc + wat_bal_hour%esten%h_perc1
                bal%lyr(2)%h_soil = wat_bal_hour%inten%h_soil2
                bal%lyr(2)%h_transp_act = bal%lyr(2)%h_transp_act + wat_bal_hour%esten%h_transp_act2
                bal%lyr(2)%h_transp_pot = bal%lyr(2)%h_transp_pot + wat_bal_hour%esten%h_transp_pot2
                bal%lyr(2)%h_perc = bal%lyr(2)%h_perc + wat_bal_hour%esten%h_perc2
                bal%k_s = wat_bal_hour%inten%k_s
                bal%h_caprise = bal%h_caprise + wat_bal_hour%esten%h_caprise
                bal%h_rise = bal%h_rise + wat_bal_hour%esten%h_rise
                wat_bal_hour%inten%h_pond0 = wat_bal_hour%esten%h_pond


                ! update the number of iterations
                iter1 = merge(iter1,wat_bal_hour%n_iter1,iter1>wat_bal_hour%n_iter1)
                iter2 = merge(iter2,wat_bal_hour%n_iter2,iter2>wat_bal_hour%n_iter2)

                if (pars%sim%prt_debug_out == 'y') then
                    ! print the number of iteration for each control cells
                    if(pars%sim%f_out_cells .eqv. .true.)then
                        do i=1,size(out_tbl_list%cell_conv)
                            xx=out_tbl_list%cell_conv(i)%coord%row
                            yy=out_tbl_list%cell_conv(i)%coord%col
                            write(out_tbl_list%cell_conv(i)%file%unit,'(i3,a1,i2,a1,(2(i2,a1,i4,a1)))')doy,'; ',hour,'; ',&
                                & wat_bal_hour%n_max1(xx,yy), '; ', wat_bal_hour%n_iter1(xx,yy), '; ',&
                                & wat_bal_hour%n_max2(xx,yy), '; ',wat_bal_hour%n_iter2(xx,yy)
                        end do
                    end if
                end if
            end do hr_loop

            ! update volumetric water content
            where(info_spat%domain%mat /= info_spat%domain%header%nan)
                bal%lyr(1)%t_soil = bal%lyr(1)%h_soil / (1000._dp * bal%lyr(1)%thickness)
                where (bal%lyr(2)%thickness > 0._dp)
                    bal%lyr(2)%t_soil = bal%lyr(2)%h_soil / (1000._dp * bal%lyr(2)%thickness)
                elsewhere !%PS%: assume layer 2 is at field capacity if unexplored
                    bal%lyr(2)%t_soil = info_spat%theta(2)%fc%mat
                end where
            end where
            if (doy == last_simulated_doy) then
                where(info_spat%domain%mat /= info_spat%domain%header%nan)
                    info_spat%theta(1)%old%mat = bal%lyr(1)%t_soil
                    info_spat%theta(2)%old%mat = bal%lyr(2)%t_soil
                end where
            end if

            ! update the ponding variable for each day
            bal%h_runoff = bal%h_runoff + max(wat_bal_hour%esten%h_pond-info_spat%h_maxpond%mat,0.0D0)
            bal%h_pond = min(wat_bal_hour%esten%h_pond,info_spat%h_maxpond%mat)
            !wat_bal1%h_pond = wat_bal_hour%esten%h_pond

            call accumulate_daily_yield(crop_yield_state, pheno, crops, meteo, bal, info_spat%domain)
            call finalize_harvested_yield(crop_yield_state, yield, pheno%harvest_pending,     &
                                        & crops, pars%sim%co2_conc(year_idx), info_spat%domain)

            call write_daily_output (doy, meteo, info_meteo, pheno, h_irr_sum, bal, bal_old, info_spat,               &
                                   & pars, wat, wat_bal_hour, fw_day, fw_old, esp_perc, out_cn, out_cn_day, h_bypass, &
                                   & coll_irr, priv_irr, out_tbl_list, pars%sim%mode, pars%sim%f_out_cells, pars%sim  )

            ! save output files by step
            call write_outputs_by_step(doy, meteo, h_irr_sum, bal, info_spat, coll_irr, priv_irr, stp_map,                &
                                     & deb_map, h_bypass, out_steps, days_before_1st_interval, last_simulated_doy, summary)

            if (coupling%enabled) then
                call accumulate_mf_fluxes(coupling, bal%lyr(2)%h_perc, bal%h_caprise, priv_irr)
                if (coupling_is_due(coupling, simulation_year_idx == n_simulation_years .and. &
                                              doy == last_simulated_doy)) then
                    call coupling_handshake(coupling, info_spat%domain, pars%sim, boundaries, &
                                          & info_spat%wat_tab, pars%depth%ze_fix)
                end if
            end if

            ! save to file the output bu year
            yr_map%rain%mat = yr_map%rain%mat + meteo%p
            yr_map%runoff%mat = yr_map%runoff%mat + bal%h_runoff
            yr_map%net_flux_gw%mat = yr_map%net_flux_gw%mat + bal%lyr(2)%h_perc - bal%h_caprise

            ! VERY IMPORTANT EDIT
            ! %EAC%: water release for irrigation should be already controlled by the presence of crop in field
            ! otherwise is an error
            yr_map%irr%mat = yr_map%irr%mat + h_irr_sum + h_bypass
            yr_map%irr_loss%mat = yr_map%irr_loss%mat + h_bypass

            where(pheno%k_cb/=0)
                yr_map%rain_crop_season%mat = yr_map%rain_crop_season%mat + meteo%p
                yr_map%eva_pot_crop_season%mat = yr_map%eva_pot_crop_season%mat + bal%h_eva_pot
                yr_map%eva_act_crop_season%mat = yr_map%eva_act_crop_season%mat + bal%h_eva
                yr_map%transp_act%mat = yr_map%transp_act%mat + bal%lyr(1)%h_transp_act + bal%lyr(2)%h_transp_act
                yr_map%transp_pot%mat = yr_map%transp_pot%mat + bal%lyr(1)%h_transp_pot + bal%lyr(2)%h_transp_pot
            end where

            yr_deb_map%eva_act_tot%mat = yr_deb_map%eva_act_tot%mat + bal%h_eva
            yr_deb_map%iter1%mat = merge(yr_deb_map%iter1%mat,dble(iter1),iter1<yr_deb_map%iter1%mat)
            yr_deb_map%iter2%mat = merge(yr_deb_map%iter2%mat,dble(iter2),iter2<yr_deb_map%iter2%mat)

            call advance_calendar_date(current)

        end do day_cycle

        ! Calculate the annual efficiency for the use of the water inputs (rain and irrigation)
        where ((yr_map%rain_crop_season%mat + yr_map%irr%mat) > 0)
            yr_map%total_eff%mat = (yr_map%eva_act_crop_season%mat + yr_map%transp_act%mat) &
                & / (yr_map%rain_crop_season%mat + yr_map%irr%mat)
        elsewhere
            yr_map%total_eff%mat = nan_r
        end where

        where (yr_map%n_irr_events%mat>0)
            yr_map%h_irr_mean%mat = yr_map%irr%mat /  yr_map%n_irr_events%mat
        elsewhere
            yr_map%h_irr_mean%mat = nan_r
        end where

        if (summary .eqv. .false.) then
            call save_yearly_data(yr_map,info_spat%domain)
        else
            call save_annual_irrigation_data(yr_map,info_spat%domain)
        end if

        call save_yield_data(yield,info_spat%domain)

        where (yr_map%rain_crop_season%mat > 0)
            yr_deb_map%rain_eff%mat = (yr_map%eva_act_crop_season%mat + yr_map%transp_act%mat) / yr_map%rain_crop_season%mat
        elsewhere
            yr_deb_map%rain_eff%mat = nan_r
        end where

        call save_annual_debug_data(yr_deb_map, info_spat%domain)
        call save_yield_debug_data(yield, info_spat%domain)

        ! close the csv files for cell outputs
        call close_cell_output_by_year(out_tbl_list,pars%sim%mode,pars%sim%f_out_cells, pars%sim,pars%cr%n_withdrawals)
        ! destroy annual variables
        if (pars%sim%mode ==1) call destroy_water_sources_duty(info_sources)
        call destroy_yield(yield)
    end do year_cycle

    ! Save output for the following year
    info_spat%theta(1)%old%mat = bal%lyr(1)%h_soil/(1000._dp*bal%lyr(1)%thickness)
    info_spat%theta(2)%old%mat = bal%lyr(2)%h_soil/(1000._dp*bal%lyr(2)%thickness)
    if (pars%sim%f_theta_out .eqv. .true.) then
        call write_grid(trim(pars%sim%final_condition)//trim(pars%sim%thetaI_end_fn)//'.asc',info_spat%theta(1)%old)
        call write_grid(trim(pars%sim%final_condition)//trim(pars%sim%thetaII_end_fn)//'.asc',info_spat%theta(2)%old)
    end if

    crop_state = pheno
    call destroy_all(stp_map, yr_map, deb_map, yr_deb_map, bal, bal_old,          &
                   & wat_bal_hour, meteo, wat, pheno, pars%sim%imax, pars%sim%jmax)

end subroutine simulation_manager

! Divide a calendar year into intervals ending on weekly_output_weekday.
function weekly_interval_days(weekly_output_weekday, year) result(interval_days)
    integer, intent(in) :: weekly_output_weekday, year
    integer, allocatable :: interval_days(:)
    integer, parameter :: days_per_week = 7
    integer :: first_interval_days, remaining_days, n_intervals, interval_idx, year_days

    year_days = days_in_year(year)
    first_interval_days = modulo(weekly_output_weekday - day_of_week(1, 1, year), days_per_week) + 1
    n_intervals = 1 + (year_days - first_interval_days + days_per_week - 1) / days_per_week
    allocate(interval_days(n_intervals))

    remaining_days = year_days
    do interval_idx = 1, n_intervals
        if (interval_idx == 1) then
            interval_days(interval_idx) = first_interval_days
        else
            interval_days(interval_idx) = min(days_per_week, remaining_days)
        end if
        remaining_days = remaining_days - interval_days(interval_idx)
    end do
end function weekly_interval_days

function periodic_interval_days(period_start_doy, period_end_doy, period_days) result(interval_days)
    integer, intent(in) :: period_start_doy, period_end_doy, period_days
    integer, allocatable :: interval_days(:)
    integer :: n_output_days, n_intervals

    if (period_days <= 0) error stop 'The periodic output interval length must be positive.'
    if (period_end_doy < period_start_doy) error stop 'The periodic output end day must not precede its start day.'

    n_output_days = period_end_doy - period_start_doy + 1
    n_intervals = (n_output_days + period_days - 1) / period_days
    allocate(interval_days(n_intervals))
    interval_days = period_days
    interval_days(n_intervals) = n_output_days - period_days * (n_intervals - 1)
end function periodic_interval_days

subroutine update_yearly_spatial_data(pars, info_spat, wat_src_tbl, irr_units, boundaries, year, alpha_ms_map, &
                                    & alpha_unm_map, fw_irr, a_loss, b_loss, c_loss, f_interception            )

    type(parameters), intent(inout) :: pars
    type(spatial_info), intent(inout) :: info_spat
    type(water_sources_table), dimension(:), intent(inout) :: wat_src_tbl
    type(irr_units_table), dimension(:), allocatable, intent(inout) :: irr_units
    type(bound), intent(in) :: boundaries
    integer, intent(in) :: year
    real(dp), dimension(:,:), intent(inout) :: alpha_ms_map, alpha_unm_map
    real(dp), dimension(:,:), intent(inout) :: fw_irr
    real(dp), dimension(:,:), intent(inout) :: a_loss, b_loss, c_loss
    real(dp), dimension(:,:), intent(inout) :: f_interception

    character(len=255) :: irandom_file, landuse_file
    character(len=255) :: yearly_irr_meth_map, yearly_irr_eff_map
    logical :: file_exists

    ! Change the crop-emergence randomization map (if one exists for this period).
    irandom_file = trim(pars%sim%input_path)//trim(pars%sim%irandom_fn)//'_'//itoa(year)//'.asc'
    inquire(file=trim(irandom_file), exist=pars%sim%f_irandom)
    if (pars%sim%f_irandom) then
        print *, 'Reading irandom values from: ', trim(irandom_file)
        call read_grid(trim(irandom_file), info_spat%irandom, pars%sim, boundaries)
    end if

    ! Change soil use and irrigation method maps (if they exist for this period)
    if (.not. pars%sim%f_soiluse) return

    ! Restore the complete domain
    info_spat%domain = info_spat%backup_domain
    info_spat%domain%mat = info_spat%backup_domain%mat

    ! %PS%: a missing landuse yearly file is allowed, in that case we reuse last year's
    landuse_file = trim(pars%sim%input_path)//trim(pars%sim%soiluse_fn)//'_'//itoa(year)//'.asc'
    inquire(file=trim(landuse_file), exist=file_exists)
    if (file_exists) then
        call read_grid(trim(landuse_file), info_spat%soil_use_id, pars%sim, boundaries)
    else
        print *, 'Landuse file for year ', itoa(year), ' is missing. Relying on the previous year instead.'
    end if

    !%PS%, todo: this check is duplicated from check_grid; consider merging them
    if (minval(info_spat%soil_use_id%mat, info_spat%soil_use_id%mat /= info_spat%soil_use_id%header%nan) < 1 &
        & .or. maxval(info_spat%soil_use_id%mat) > pars%sim%n_lus) then
        print *, 'Soil use maps have soil uses not defined in crop database'
        print *, 'Verify the landuse list at ', trim(join_path(pars%sim%crop_inputs_path, pars%sim%soil_uses_fn, delimiter))
        print *, 'Execution will be aborted...'
        stop
    end if

    call overlay_domain(info_spat%soil_use_id, info_spat%domain, trim(landuse_file)) ! Remove NaNs from the domain

    ! Irrigation method - todo: add indipendent flag; should not follow pars%sim%f_soiluse
    if (pars%sim%mode > 0) then
        yearly_irr_meth_map = trim(pars%sim%id_irr_meth_fn)//'_'//itoa(year)//'.asc'
        call read_grid(trim(pars%sim%input_path)//yearly_irr_meth_map, info_spat%irr_meth_id, &
            & pars%sim, boundaries) ! TODO: check if file exists
        call validate_irr_method_map(info_spat%irr_meth_id, info_spat%domain, pars%sim%n_irr_meth, &
            & yearly_irr_meth_map)

        ! Spreads parameters across the simulation domain according to each cell's method
        !%PS%: note that a/b/c_losses and h_meth are not used in mode 2, but setting them anyway is safe and makes for clearer code
        alpha_ms_map = id_to_par(info_spat%irr_meth_id, pars%irr%met(:)%irr_th_ms)
        alpha_unm_map = id_to_par(info_spat%irr_meth_id, pars%irr%met(:)%irr_th_unm)
        fw_irr = id_to_par(info_spat%irr_meth_id, pars%irr%met(:)%f_wet)
        a_loss = id_to_par(info_spat%irr_meth_id, pars%irr%met(:)%a_loss)
        b_loss = id_to_par(info_spat%irr_meth_id, pars%irr%met(:)%b_loss)
        c_loss = id_to_par(info_spat%irr_meth_id, pars%irr%met(:)%c_loss)
        f_interception = id_to_par(info_spat%irr_meth_id, dble(pars%irr%met(:)%f_interception))

        info_spat%h_meth = info_spat%domain
        info_spat%h_meth%mat = id_to_par(info_spat%irr_meth_id, pars%irr%met(:)%h_irr)
        info_spat%irr_starts%mat = id_to_par(info_spat%irr_meth_id, pars%irr%met(:)%irr_starts)
        info_spat%irr_ends%mat = id_to_par(info_spat%irr_meth_id, pars%irr%met(:)%irr_ends)

        call calc_perc_booster_pars(info_spat, pars%irr%met, pars%sim%quantiles)

        ! Only update irrigation units in mode 1
        if (pars%sim%mode == 1) then
            call init_irrigation_units(info_spat%domain, info_spat%irr_unit_id, info_spat%eff_net, &
                                     & irr_units, wat_src_tbl, pars, info_spat%h_meth              )
        end if

        ! Only read efficiency maps in modes 2 and 4
        if (pars%sim%mode == 2 .or. pars%sim%mode == 4) then
            yearly_irr_eff_map = trim(pars%sim%eff_irr_fn)//'_'//itoa(year)//'.asc'
            call read_grid(trim(pars%sim%input_path)//yearly_irr_eff_map, info_spat%eff_met, pars%sim, boundaries)
            call set_default_par(info_spat%eff_met, info_spat%domain, 1.0D0)
        end if
    end if

    ! Debug output
    if (pars%sim%prt_debug_out == 'y') then
        call write_grid(trim(pars%sim%path)//'out_'//trim(pars%sim%soiluse_fn)//'_'//itoa(year)//'.asc', info_spat%soil_use_id)
        if (pars%sim%mode > 0) then
            call write_grid(trim(pars%sim%path)//'out_'//yearly_irr_meth_map, info_spat%irr_meth_id)
            if (pars%sim%mode == 2 .or. pars%sim%mode == 4) then
                call write_grid(trim(pars%sim%path)//'out_'//yearly_irr_eff_map, info_spat%eff_met)
            end if
        end if
    end if

end subroutine update_yearly_spatial_data

subroutine initialize_yearly_crop_state(pars, year, info_spat, rotations, yield)
    type(parameters), intent(in) :: pars
    integer, intent(in) :: year
    type(spatial_info), intent(inout) :: info_spat
    type(crop_rotation), intent(in) :: rotations(:)
    type(yield_t), intent(inout) :: yield
    integer :: rotation_idx, output_slots

    ! Without an irandom map, sample a sowing offset for each cell.
    if (.not. pars%sim%f_irandom) then
        call get_uniform_sample(info_spat%irandom%mat, pars%sim%sowing_range, pars%sim%rand_symmetry, pars%sim%repeatable)
    end if

    output_slots = 1
    do rotation_idx = 1, size(rotations)
        if (allocated(rotations(rotation_idx)%crop_ids)) output_slots = max(output_slots, size(rotations(rotation_idx)%crop_ids))
    end do
    call initialize_yield(yield, info_spat%domain%mat, output_slots)

    if (pars%sim%prt_debug_out == 'y') then
        call print_mat_as_grid(trim(pars%sim%path)//itoa(year)//'_irandom.asc', info_spat%irandom%header, info_spat%irandom%mat)
    end if
end subroutine initialize_yearly_crop_state

! Initialization of yearly *.csv output files
subroutine initialize_yearly_outputs(pars, year, info_meteo, info_spat, irr_units, out_tbl_list, yr_map, yield, yr_deb_map)

    type(parameters), intent(in) :: pars
    integer, intent(in) :: year
    type(meteo_info), dimension(:), intent(in) :: info_meteo
    type(spatial_info), intent(in) :: info_spat
    type(irr_units_table), dimension(:), allocatable, intent(in) :: irr_units
    type(output_table_list), intent(inout) :: out_tbl_list
    type(annual_map), intent(inout) :: yr_map
    type(yield_t), intent(inout) :: yield
    type(annual_debug_map), intent(inout) :: yr_deb_map

    if (pars%sim%mode == 1) then
        call init_cell_output_by_year(out_tbl_list, pars%sim%path, itoa(year), info_meteo%filename,                     &
                                    & pars%sim%mode, pars%sim%f_out_cells, pars%sim, irr_units%id, pars%cr%n_withdrawals)
    else
        call init_cell_output_by_year(out_tbl_list, pars%sim%path, itoa(year), info_meteo%filename, &
                                    & pars%sim%mode, pars%sim%f_out_cells, pars%sim                 )
    end if

    if(pars%sim%f_out_cells)then
        call write_cell_info(info_spat, out_tbl_list%cell_info, pars%sim%mode, pars%sim%f_cap_rise, &
                           & pars%depth%ze_fix, pars%depth%zr_fix, year                )
    end if

    call init_yearly_output_file(yr_map, pars%sim%path, itoa(year), pars%sim)
    call init_yield_output_file(yield, pars%sim%path, itoa(year), pars%sim)
    call init_debug_yearly_output_file(yr_deb_map, pars%sim%path, itoa(year), pars%sim)

end subroutine initialize_yearly_outputs

subroutine update_water_table(pars, info_spat, boundaries, year, doy)
    type(parameters), intent(in) :: pars
    type(spatial_info), intent(inout) :: info_spat
    type(bound), intent(in) :: boundaries
    integer, intent(in) :: year, doy
    character(len=255) :: file
    logical :: file_exists

    ! Looks for a '*_year_doy' map first; falls back to "*_yyyy_doy" if missing
    file = trim(pars%sim%input_path)//trim(pars%sim%wat_table_fn)//"_"//itoa(year)//"_"//itoa(doy)//".asc"
    inquire(file=trim(file), exist=file_exists)
    if (.not. file_exists) then
        file = trim(pars%sim%input_path)//trim(pars%sim%wat_table_fn)//"_"//"yyyy"//"_"//itoa(doy)//".asc" !%PS%, todo: is this kind of filename ever used?
        inquire(file=trim(file), exist=file_exists)
        if (.not. file_exists) return
    end if

    ! File is present: imports the data
    print *, 'Reading updated water table data from: ', file
    call read_grid(trim(file), info_spat%wat_tab, pars%sim, boundaries)

    ! Ensures the water table can't enter the first soil layer
    where((info_spat%wat_tab%mat < pars%depth%ze_fix) .and. (info_spat%wat_tab%mat /= info_spat%wat_tab%header%nan))
        info_spat%wat_tab%mat = pars%depth%ze_fix
    end where
end subroutine update_water_table

subroutine update_crop_map(pars, crop_map, domain, boundaries, crops, year, doy)
    type(parameters), intent(in) :: pars
    type(grid_i), intent(inout) :: crop_map
    type(grid_i), intent(in) :: domain
    type(bound), intent(in) :: boundaries
    type(crop_definition), intent(in) :: crops(:)
    integer, intent(in) :: year, doy
    character(len=255) :: file
    logical :: file_exists
    integer :: i, j, crop_id

    file = trim(pars%sim%input_path)//'crop_id_'//itoa(year)//'_'//itoa(doy)//'.asc'
    inquire(file=trim(file), exist=file_exists)
    if (.not. file_exists) return ! A missing file maintains the previously read map

    print *, 'Reading updated crop ID data from: ', trim(file)
    call read_grid(trim(file), crop_map, pars%sim, boundaries)

    do j = 1, size(crop_map%mat, 2)
        do i = 1, size(crop_map%mat, 1)
            if (domain%mat(i,j) == domain%header%nan) cycle
            crop_id = crop_map%mat(i,j)
            if (crop_id < 0 .or. crop_id > size(crops)) then
                print *, 'Invalid crop ID in ', trim(file), ' at row/column ', i, j, ': ', crop_id
                print *, 'Execution will be aborted...'
                stop
            end if
            if (crop_id > 0) then
                if (.not. allocated(crops(crop_id)%gdd)) then
                    print *, 'Crop ID ', crop_id, ' in ', trim(file), ' has no loaded crop definition.'
                    print *, 'Execution will be aborted...'
                    stop
                end if
            end if
        end do
    end do
end subroutine update_crop_map

subroutine write_daily_output(doy, meteo, info_meteo, pheno, h_irr_sum, balance, balance_old,    &
                            & info_spat, pars, wat, wat_bal_hour, fw, fw_old, esp_perc, out_cn,  &
                            & out_cn_day, h_bypass, coll_irr, priv_irr, out_tbl, mode, cells, sim)
    ! write daily output for each control cells
   integer, intent(in):: doy
    type(meteo_mat), intent(in):: meteo
    type(meteo_info), dimension(:), intent(in):: info_meteo
    type(crop_pars_matrices), intent(in):: pheno
    real(dp), dimension(:,:), intent(in):: h_irr_sum
    type(balance_matrices), intent(in):: balance, balance_old
    type(spatial_info), intent(in):: info_spat
    type(parameters), intent(in)::pars
    real(dp), dimension(:,:), intent(in)::fw, fw_old ! %RR% test
    real(dp), dimension(:,:,:), intent(in):: esp_perc
    type(output_cn),dimension(:,:),intent(in)::out_cn
    real(dp),dimension(:,:),intent(in)::out_cn_day
    real(dp), dimension(:,:), intent(in):: h_bypass
    real(dp), dimension(:,:), intent(in):: coll_irr, priv_irr
    type(output_table_list), intent(in):: out_tbl
    integer, intent(in)::mode
    logical, intent(in)::cells
    type(simulation),intent(in)::sim

    type(wat_matrix)::wat
    type(hourly)::wat_bal_hour
    integer::i
    integer::xx,yy

    if (cells .eqv. .true.) then
        do i=1,size(out_tbl%sample_cells)
            xx=out_tbl%sample_cells(i)%coord%row
            yy=out_tbl%sample_cells(i)%coord%col
            select case (mode)
                case (0, 2:4)
                    write(out_tbl%sample_cells(i)%file%unit,*)doy, &
                        & ';', meteo%p(xx,yy), ';', meteo%T_max(xx,yy), &
                        & ';', meteo%T_min(xx,yy), ';', meteo%et0(xx,yy), &
                        & ';', pheno%k_cb(xx,yy), ';',pheno%lai(xx,yy), ';',pheno%p_day(xx,yy), &
                        & ';', h_irr_sum(xx,yy),&
                        & ';', balance%h_eff_rain(xx,yy), ';', balance%h_gross_av_water(xx,yy), ';',balance%h_net_av_water(xx,yy), &
                        & ';', balance%h_interc(xx,yy), ';',balance%h_runoff(xx,yy), ';',balance%h_inf(xx,yy), &
                        & ';', balance%h_eva_pot(xx,yy), ';',balance%h_eva(xx,yy), &
                        & ';', balance%lyr(1)%h_transp_pot(xx,yy), ';',balance%lyr(1)%h_transp_act(xx,yy), &
                        & ';', balance%lyr(1)%h_perc(xx,yy), ';',balance%lyr(1)%h_soil(xx,yy), &
                        & ';', balance%h_pond(xx,yy), ';', balance%h_rise(xx,yy), &
                        & ';', balance%lyr(2)%h_transp_pot(xx,yy), ';',balance%lyr(2)%h_transp_act(xx,yy), ';',balance%k_s(xx,yy), &
                        & ';', balance%lyr(2)%thickness(xx,yy), ';',balance%depth_under_rz(xx,yy), ';',balance%h_caprise(xx,yy), &
                        & ';', balance%lyr(2)%h_perc(xx,yy), ';',balance%lyr(2)%h_soil(xx,yy),';',balance_old%lyr(2)%h_soil(xx,yy), &
                        & ';', balance%h_irr_thresh(xx,yy), &
                        & ';', 0._dp, & !%PS%: rawinf is no longer calculated, replaced with "0". Remove this column when refactoring this output file
                        & ';', info_spat%wat_tab%mat(xx,yy), &
                        & ';', 0,';',0, & ! add dummy variables to maintain file structure
                        & ';', esp_perc(xx,yy,1),';',esp_perc(xx,yy,2),';',h_bypass(xx,yy), &
                        ! new variables
                        & ';', pheno%RF_e(xx,yy),';',pheno%RF_t(xx,yy),';',pheno%r_stress(xx,yy), &
                        & ';', wat%layer(2)%h_r(xx,yy),';',wat%layer(2)%h_wp(xx,yy),';',wat%layer(2)%h_fc(xx,yy),';',wat%layer(2)%h_sat(xx,yy), &
                        & ';', info_spat%h_maxpond%mat(xx,yy)

                case (1)
                    write(out_tbl%sample_cells(i)%file%unit,*)doy, &
                        & ';', meteo%p(xx,yy), ';', meteo%T_max(xx,yy), &
                        & ';', meteo%T_min(xx,yy), ';', meteo%et0(xx,yy), &
                        & ';', pheno%k_cb(xx,yy), ';',pheno%lai(xx,yy), ';',pheno%p_day(xx,yy), &
                        & ';', h_irr_sum(xx,yy),&
                        & ';', balance%h_eff_rain(xx,yy), ';', balance%h_gross_av_water(xx,yy), ';',balance%h_net_av_water(xx,yy), &
                        & ';', balance%h_interc(xx,yy), ';',balance%h_runoff(xx,yy), ';',balance%h_inf(xx,yy), &
                        & ';', balance%h_eva_pot(xx,yy), ';',balance%h_eva(xx,yy), &
                        & ';', balance%lyr(1)%h_transp_pot(xx,yy), ';',balance%lyr(1)%h_transp_act(xx,yy), &
                        & ';', balance%lyr(1)%h_perc(xx,yy), ';',balance%lyr(1)%h_soil(xx,yy), &
                        & ';', balance%h_pond(xx,yy), ';', balance%h_rise(xx,yy), &
                        & ';', balance%lyr(2)%h_transp_pot(xx,yy), ';',balance%lyr(2)%h_transp_act(xx,yy), ';',balance%k_s(xx,yy), &
                        & ';', balance%lyr(2)%thickness(xx,yy), ';',balance%depth_under_rz(xx,yy), ';',balance%h_caprise(xx,yy), &
                        & ';', balance%lyr(2)%h_perc(xx,yy), ';',balance%lyr(2)%h_soil(xx,yy),';',balance_old%lyr(2)%h_soil(xx,yy), &
                        & ';', balance%h_irr_thresh(xx,yy), &
                        & ';', 0._dp, & ! Legacy rawinf column retained for downstream positional consumers
                        & ';', info_spat%wat_tab%mat(xx,yy), &
                        & ';', coll_irr(xx,yy),';',priv_irr(xx,yy),';',esp_perc(xx,yy,1),&
                        & ';', esp_perc(xx,yy,2),';',h_bypass(xx,yy),&
                        ! new variables
                        & ';', pheno%RF_e(xx,yy),';',pheno%RF_t(xx,yy),';',pheno%r_stress(xx,yy), &
                        & ';', wat%layer(2)%h_r(xx,yy),';',wat%layer(2)%h_wp(xx,yy),';',wat%layer(2)%h_fc(xx,yy),';',wat%layer(2)%h_sat(xx,yy), &
                        & ';', info_spat%h_maxpond%mat(xx,yy)
                case default
            end select
        end do
    end if

    if (sim%prt_cell_et0 =='y') then
        write(out_tbl%et0_ws%unit,*) doy,'; ',(info_meteo(i)%et0,'; ',i=1,size(info_meteo))
    end if

    if (sim%prt_cell_evaporation =='y') then
        if (cells .eqv. .true.) then
            do i=1,size(out_tbl%cell_eva)
                xx=out_tbl%cell_eva(i)%coord%row
                yy=out_tbl%cell_eva(i)%coord%col
                if (h_irr_sum(xx,yy)/=0) then
                     write(out_tbl%cell_eva(i)%file%unit,*)doy, &
                        & ';', meteo%Wind_vel(xx,yy), ';', meteo%RH_min(xx,yy), &
                        & ';', meteo%et0(xx,yy), &
                        & ';', pheno%k_cb(xx,yy), ';', pheno%h(xx,yy), &
                        & ';', pars%irr%met(info_spat%irr_meth_id%mat(xx,yy))%f_wet, ';', wat%few(xx,yy), &
                        & ';', pheno%f_c(xx,yy), &
                        & ';', wat%kc_max(xx,yy), &
                        & ';', wat%wat1_rew(xx,yy), ';', wat%layer(1)%h_wp(xx,yy), ';', wat_bal_hour%inten%h_soil1(xx,yy), &
                        & ';', wat_bal_hour%esten%k_e(xx,yy), &
                        & ';', balance%h_eva_pot(xx,yy), ';', balance%h_eva(xx,yy), ';', wat_bal_hour%esten%k_r(xx,yy), ';', fw_old(xx,yy) ! RR test
                else
                     write(out_tbl%cell_eva(i)%file%unit,*)doy, &
                        & ';', meteo%Wind_vel(xx,yy), ';', meteo%RH_min(xx,yy), &
                        & ';', meteo%et0(xx,yy), &
                        & ';', pheno%k_cb(xx,yy), ';', pheno%h(xx,yy), &
                        & ';', fw(xx,yy), ';', wat%few(xx,yy), &
                        & ';', pheno%f_c(xx,yy), &
                        & ';', wat%kc_max(xx,yy), &
                        & ';', wat%wat1_rew(xx,yy), ';', wat%layer(1)%h_wp(xx,yy), ';', wat_bal_hour%inten%h_soil1(xx,yy), &
                        & ';', wat_bal_hour%esten%k_e(xx,yy), &
                        & ';', balance%h_eva_pot(xx,yy), ';', balance%h_eva(xx,yy), ';', wat_bal_hour%esten%k_r(xx,yy), ';', fw_old(xx,yy) !%RR% test
                end if
            end do
        end if
    end if

    if (sim%prt_cell_runoff =='y') then
        if (cells .eqv. .true.) then
            do i=1, size(out_tbl%cell_cn)
                xx=out_tbl%cell_cn(i)%coord%row
                yy=out_tbl%cell_cn(i)%coord%col
                write(out_tbl%cell_cn(i)%file%unit,*)doy, &
                    & ';', info_spat%theta(1)%wp%mat(xx,yy)+info_spat%theta(2)%wp%mat(xx,yy), &
                    & ';', info_spat%theta(1)%fc%mat(xx,yy)+info_spat%theta(2)%fc%mat(xx,yy), &
                    & ';', info_spat%theta(1)%sat%mat(xx,yy)+info_spat%theta(2)%sat%mat(xx,yy), &
                    & ';', info_spat%theta(1)%wp%mat(xx,yy)+info_spat%theta(2)%wp%mat(xx,yy)+ &
                    & (2.0/3.0)*(info_spat%theta(1)%fc%mat(xx,yy)+info_spat%theta(2)%fc%mat(xx,yy)- &
                    & info_spat%theta(1)%wp%mat(xx,yy)-info_spat%theta(2)%wp%mat(xx,yy)), &
                    & ';', balance%lyr(1)%t_soil(xx,yy)+balance%lyr(2)%t_soil(xx,yy), &
                    & ';', out_cn(xx,yy)%tab_cn2, ';', out_cn(xx,yy)%tab_cn3, &
                    & ';', info_spat%slope%mat(xx,yy), &
                    & ';', pheno%cn_class(xx,yy),';', pheno%cn_day(xx,yy), &
                    & ';', out_cn(xx,yy)%tab_cn2_baresoil, ';', out_cn(xx,yy)%tab_cn2_slope, &
                    & ';', out_cn(xx,yy)%cn1_day, ';', out_cn(xx,yy)%cn2_day, &
                    & ';', out_cn(xx,yy)%cn3_day, ';', out_cn(xx,yy)%cn_day, &
                    & ';', pars%sim%lambda_cn, &
                    & ';', 25.4*((1000./out_cn_day(xx,yy))-10.), &
                    & ';', pars%sim%lambda_cn*25.4*((1000./out_cn_day(xx,yy))-10.), &
                    & ';', balance%h_gross_av_water(xx,yy), ';', balance%h_net_av_water(xx,yy), &
                    & ';', ((balance%h_gross_av_water(xx,yy)-pars%sim%lambda_cn*25.4*((1000./out_cn_day(xx,yy))-10.))**2.)/ &
                    & (balance%h_gross_av_water(xx,yy)+0.8*25.4*((1000./out_cn_day(xx,yy))-10.)), &
                    & ';', balance%h_runoff(xx,yy)
            end do
        end if
    end if
end subroutine write_daily_output

subroutine write_outputs_by_step(doy, meteo, irrigation_sum, balance, info_spat, coll_irr, priv_irr, asc, deb_asc, &
                               & hbypass, interval_days, days_before_1st_interval, last_simulated_doy, summary     )
    ! writes periodic (monthly/weekly/custom) output in *.asc files
    integer, intent(in) :: doy
    type(meteo_mat), intent(in):: meteo
    real(dp), dimension(:,:), intent(in):: irrigation_sum
    real(dp), dimension(:,:), intent(in):: hbypass
    type(balance_matrices), intent(in) :: balance
    type(spatial_info), intent(in):: info_spat
    real(dp), dimension(:,:), intent(in):: coll_irr, priv_irr
    type(step_map), intent(inout):: asc
    type(step_debug_map), intent(inout):: deb_asc
    integer, dimension(:), intent(in) :: interval_days
    integer, intent(in) :: days_before_1st_interval
    integer, intent(in) :: last_simulated_doy
    logical, intent(in)::summary

    asc%runoff%mat = asc%runoff%mat + balance%h_runoff
    asc%rain%mat = asc%rain%mat + meteo%p
    asc%transp_act%mat = asc%transp_act%mat + balance%lyr(1)%h_transp_act + balance%lyr(2)%h_transp_act
    asc%transp_pot%mat = asc%transp_pot%mat + balance%lyr(1)%h_transp_pot + balance%lyr(2)%h_transp_pot
    asc%irr%mat = asc%irr%mat + irrigation_sum + hbypass
    asc%irr_loss%mat = asc%irr_loss%mat + hbypass
    asc%cap_rise%mat = asc%cap_rise%mat + balance%h_caprise
    asc%irr_nm_priv%mat = asc%irr_nm_priv%mat + priv_irr
    asc%irr_nm_col%mat = asc%irr_nm_col%mat + coll_irr
    asc%deep_perc%mat = asc%deep_perc%mat + balance%lyr(2)%h_perc - balance%h_caprise
    asc%et_pot%mat = asc%et_pot%mat + balance%h_eva_pot + balance%lyr(1)%h_transp_pot + balance%lyr(2)%h_transp_pot
    asc%et_act%mat = asc%et_act%mat + balance%h_eva + balance%lyr(1)%h_transp_act + balance%lyr(2)%h_transp_act

    deb_asc%eva_act%mat = deb_asc%eva_act%mat + balance%h_eva
    deb_asc%eff_rain%mat = deb_asc%eff_rain%mat + balance%h_eff_rain
    deb_asc%perc1%mat = deb_asc%perc1%mat + balance%lyr(1)%h_perc
    deb_asc%perc2%mat = deb_asc%perc2%mat + balance%lyr(2)%h_perc
    deb_asc%h_soil1%mat = balance%lyr(1)%h_soil
    deb_asc%h_soil2%mat = balance%lyr(2)%h_soil

    if (summary .eqv. .false.) then
        call save_step_data(asc, doy, info_spat%domain, interval_days, days_before_1st_interval, last_simulated_doy)
    else
        call save_step_irrigation(asc, doy, info_spat%domain, interval_days, days_before_1st_interval, last_simulated_doy)
    end if

    call save_debug_step_data(deb_asc, doy, info_spat%domain, interval_days, days_before_1st_interval, last_simulated_doy)

end subroutine write_outputs_by_step

subroutine allocate_all(asc, yasc, deb_asc, deb_yasc, balance, balance_old, bil_hour, meteo, wat, pheno, imax, jmax,domain,crops)
    integer,dimension(:,:),intent(in)::domain
    type(crop_definition), intent(in) :: crops(:)
    type(step_map)::asc
    type(step_debug_map):: deb_asc
    type(annual_map)::yasc
    type(annual_debug_map):: deb_yasc
    type(balance_matrices) :: balance, balance_old
    type(hourly)::bil_hour
    type(meteo_mat)::meteo
    type(wat_matrix)::wat
    type(crop_pars_matrices)::pheno
    integer,intent(in)::imax!fn
    integer,intent(in)::jmax
    logical::allocazione

    allocazione = .true.
    !allocazione della variabile per gli output *.asc
    call init_step_output(asc,domain)
    call init_yearly_output(yasc,domain)
    call init_step_debug_output(deb_asc,domain)
    call init_yearly_debug_output(deb_yasc,domain)

    !alloca/dealloca le matrici in base al flag TRUE/FALSE
    call init_balance_matrices(balance,imax,jmax,allocazione)
    call init_balance_matrices(balance_old,imax,jmax,allocazione)
    call init_wat_bal_hour(bil_hour,imax, jmax, allocazione)
    call init_meteo_matrices(meteo,imax, jmax, allocazione)
    call init_wat_matrices(wat,imax, jmax, allocazione)
    call init_pheno_matrices(pheno,imax, jmax, allocazione, crops)
end subroutine allocate_all

subroutine destroy_all(asc, yasc, deb_asc, deb_yasc, balance, balance_old, bil_hour, meteo, wat, pheno, imax, jmax)
   type(step_map)::asc
    type(step_debug_map),optional::deb_asc
    type(annual_map)::yasc
    type(annual_debug_map),optional::deb_yasc
    type(balance_matrices) :: balance, balance_old
    type(hourly)::bil_hour
    type(meteo_mat)::meteo
    type(wat_matrix)::wat
    type(crop_pars_matrices)::pheno
    integer,intent(in)::imax
    integer,intent(in)::jmax
    logical::f_allocate

    f_allocate = .false.
    ! destroy reference to output files
    call destroy_step_output(asc)
    call destroy_annual_output(yasc)
    call destroy_step_debug_output(deb_asc)
    call destroy_annual_debug_output(deb_yasc)
    ! allocate/destrot the matrix base on f_allocate flag (true/false)
    call init_balance_matrices(balance,imax,jmax,f_allocate)
    call init_balance_matrices(balance_old,imax,jmax,f_allocate)
    call init_wat_bal_hour(bil_hour,imax, jmax, f_allocate)
    call init_meteo_matrices(meteo,imax, jmax, f_allocate)
    call init_wat_matrices(wat,imax, jmax, f_allocate)
    call init_pheno_matrices(pheno,imax, jmax, f_allocate)
end subroutine destroy_all

function calc_interception(p,pheno)
    ! Calculate the interception according to Von Hoyningen-Hune (1983) and Braden (1985)
   real(dp),dimension(:,:),intent(in)::p       ! precipitation and any other above canopy irrigation [mm]
    type(crop_pars_matrices),intent(in)::pheno
    real(dp),dimension(size(p,1),size(p,2))::f_c  !cover fraction [-]
    real(dp),dimension(size(p,1),size(p,2))::calc_interception

    calc_interception = 0.0D0
    where (p > 0.0D0 .and. pheno%lai > 0.0D0 .and. pheno%a > 0.0D0)
        ! Limit f_c to 1 in order to not have interception greater than precipitation.
        f_c = min(1.0D0, pheno%lai/3.0D0)
        calc_interception = (pheno%a * pheno%lai * f_c * p) / & !%PS%: Algebraically equivalent to the original formula, but avoids potentially unsafe division by a*LAI.
                            (pheno%a * pheno%lai + f_c * p)
    end where
end function calc_interception

function net_precipitation(h_gross_precip, h_interception)
    ! calculate the effective precipitation of each day
   real(dp),dimension(:,:),intent(in)::h_gross_precip                       ! precipitation + irrigation above canopy [mm]
    real(dp),dimension(size(h_gross_precip,1),size(h_gross_precip,2))::h_interception       ! interception [mm] !TODO: allocated externally?
    real(dp),dimension(size(h_gross_precip,1),size(h_gross_precip,2))::net_precipitation

    net_precipitation = 0.
    where(h_gross_precip>0.)
        net_precipitation=h_gross_precip-h_interception
        where ((net_precipitation<0.) .and. (net_precipitation>-1E-05))
            ! delete rounding error (except for big negative values that are errors!!!)
            net_precipitation=0.
        end where
    end where
end function net_precipitation

subroutine eq_extensive(a,b)
    ! overloading of the assignment operator for the variable of type hourly
   real(dp),intent(in)::b
    type(hourly),intent(out)::a

    a%esten%k_e = b
    a%esten%k_r = b
    a%esten%h_eva = b
    a%esten%h_eva_pot = b
    a%esten%h_inf = b
    a%esten%h_perc1 = b
    a%esten%h_perc2 = b
    a%esten%h_pond = b
    a%esten%h_transp_act1 = b
    a%esten%h_transp_pot1 = b
    a%esten%h_transp_act2 = b
    a%esten%h_transp_pot2 = b
    a%esten%h_caprise = b
    a%esten%h_rise = b
    a%n_iter1 = int(b)
    a%n_iter2 = int(b)
    a%n_max1 = 1
    a%n_max2 = 1
end subroutine eq_extensive

subroutine b1_no_iter_eva(pheno, meteo, h_rain_lim, wat, fw_day, fw_irr, fw_rain, fc, &
                        & h_irr_sum, f_interception, domain, balance1_mat, fw_old) ! %RR% add fw_old for testing
    ! calculate the elements of the evaporative model that change daily
   type(grid_i),intent(in)::domain
    type(balance_matrices),intent(in):: balance1_mat
    type(crop_pars_matrices),intent(inout)::pheno
    type(meteo_mat),intent(in)::meteo
    type(wat_matrix),intent(inout)::wat
    real(dp),dimension(:,:),intent(in)::h_irr_sum
    real(dp),dimension(:,:),intent(in)::f_interception
    real(dp),dimension(:,:),intent(in)::fw_irr   ! fraction of soil wetted during irrigation event
    real(dp),dimension(:,:),intent(inout)::fw_day, fw_old ! fw daily updated - %RR% test
    real(dp),intent(in):: fw_rain                ! fraction of soil wetted during a precipitation event
    real(dp),parameter::kc_min=0.175             ! minimum value of Kc of bare soil (it changes between 0.15 a 0.20)[-]
    real(dp),dimension(size(domain%mat,1),size(domain%mat,2)):: Kc_max1, Kc_max2, fc
    real(dp),dimension(size(domain%mat,1),size(domain%mat,2)):: V_eva, TEW, h_irr_net, h_rain_net, fw_day_tmp !, fw_old test
    real(dp),dimension(size(domain%mat,1),size(domain%mat,2)):: zero_mat ! to set minimum value of V_eva - %RR%
    real(dp),intent(in)::h_rain_lim !minimum meaningfull value of precipitation

    zero_mat = 0.
    fw_old = fw_day +0.
    fw_day_tmp = fw_day + 0.

    where(domain%mat/=domain%header%nan)
        ! init fw
        ! fw = fw_irr      if the surface is wetted by irrigation
        ! fw = (fw_irr*net_irrigation + fwCost*net_rain)/(net_irr+net_rain)           if the surface is wetted by irrigation and rain, fw is the weigthed average of
        !                                                                                                                                           fw_irr and fw_rain (=1) based on infiltration depths
        ! fw = 1                if the surface is wetted by significant rain (i.e. >= 5mm)
        ! if fw_old > fw_day --> weigthed average between fw_old and fw_day based on the water content of the 1st layer - Gandolfi 29/5

        h_irr_net = h_irr_sum - calc_interception(h_irr_sum*f_interception, pheno)
        h_rain_net = meteo%P - calc_interception(meteo%p, pheno)

        V_eva = max(zero_mat, (balance1_mat%lyr(1)%h_soil - wat%layer(1)%h_wp))
        TEW = wat%layer(1)%h_sat - wat%layer(1)%h_wp

        where(h_irr_sum/=0 .and. meteo%P>= h_rain_lim)
            fw_day = (fw_irr * h_irr_net+ fw_rain * h_rain_net)/(h_irr_net + h_rain_net)
        else where(h_irr_sum/=0)
            fw_day = fw_irr
        else where (meteo%P>= h_rain_lim)
            fw_day = fw_rain
        else where
            fw_day = fw_old
        end where

        fw_day_tmp = fw_day

        where(fw_old>fw_day_tmp)
            fw_day = (fw_old * V_eva + fw_day_tmp * min(V_eva + h_irr_net, TEW))/ &
            &                         (V_eva + min(V_eva + h_irr_net, TEW))
        end where

        ! FAO56 eq. 23
        ! Kc_max is the upper limit for evaporation and transpiration
        ! linked to the available energy (it ranges from 1.05 to 1.30) [-]
        ! after the precipitation or irrigation event
        Kc_max1 = 1.2+(0.04*(meteo%Wind_vel-2) - 0.004*(meteo%RH_min-45))*((pheno%h/3)**0.3)
        Kc_max2 = pheno%k_cb + 0.05 ! always greater than k_cb also in case of complete cover
        wat%kc_max = max(Kc_max1,Kc_max2)

        ! FAO56 eq. 76
        ! few  =  exposed and wetted soil fraction [-]
        ! the evaporation depends from the weetted surface and it is greater with higher fraction of bare soil
        where(pheno%f_c<0.)
            ! fc calculated with equation 76 [FAO56 p.149] unless it isn't an input %RR%
            where(pheno%k_cb<=kc_min)
                fc=0.0! %EAC% TODO: limit the exposed surface
            else where
                fc = ((pheno%k_cb-kc_min)/(wat%kc_max-kc_min))**(1+0.5*pheno%h)
            end where
        else where
            fc = pheno%f_c
        end where

        pheno%f_c = fc

        ! FAO56 eq. 75
        wat%few=min(1-fc,fw_day)
        !(3) Kr
        wat%wat1_rew=wat%layer(1)%h_fc-wat%layer(1)%rew ! water content at REW
    end where

end subroutine b1_no_iter_eva

! Make sure variables that are accumulated hourly are zeroed before the hourly loop
subroutine reset_water_balance_variables(balance)
    type(balance_matrices),intent(inout)::balance

    balance%h_inf = 0.
    balance%h_eva = 0.
    balance%h_eva_pot = 0.
    balance%lyr(1)%h_transp_act = 0.
    balance%lyr(1)%h_transp_pot = 0.
    balance%lyr(1)%h_perc = 0.
    balance%lyr(2)%h_transp_act = 0.
    balance%lyr(2)%h_transp_pot = 0.
    balance%lyr(2)%h_perc = 0.
    balance%h_caprise = 0.
    balance%h_rise = 0.
end subroutine reset_water_balance_variables

subroutine update_soil_pars(domain, theta, d_e, d_t, wat, theta2_rice, is_rice_paddy)
    ! update water matrix that change with only the thikness of the soil layer
   type(grid_i),intent(in)::domain
    type(moisture),dimension(:),intent(in)::theta
    real(dp),dimension(:,:),intent(in)::d_e,d_t
    type(wat_matrix),intent(out)::wat
    type(soil2_rice),intent(in)::theta2_rice
    logical, dimension(:,:), intent(in) :: is_rice_paddy

    where(domain%mat/=domain%header%nan)
        wat%layer(1)%h_wp  = 1000*theta(1)%wp%mat*d_e             ! water soil content at WP [mm]
        wat%layer(1)%h_fc  = 1000*theta(1)%fc%mat*d_e             ! water soil content at FC [mm]
        wat%layer(1)%h_sat = 1000*theta(1)%sat%mat*d_e            ! water soil content at saturation [mm] %AB%
        wat%layer(1)%h_r   = 1000*theta(1)%r%mat*d_e              ! water soil content at residual humidity [mm]
        wat%layer(1)%rew = ((theta(1)%fc%mat - 0.5*theta(1)%wp%mat)*0.4)*1000*d_e
        where (is_rice_paddy)
            wat%layer(2)%h_wp=1000*theta2_rice%theta2_WP*d_t     ! water soil content at WP [mm]
            wat%layer(2)%h_fc=1000*theta2_rice%theta2_FC*d_t     ! water soil content at FC [mm]
            wat%layer(2)%h_sat=1000*theta2_rice%theta2_SAT*d_t   ! water soil content at saturation [mm] %AB%
            wat%layer(2)%h_r=1000*theta2_rice%theta2_R*d_t       ! water soil content at residual humidity [mm]
        else where
            wat%layer(2)%h_wp=1000*theta(2)%wp%mat*d_t            ! water soil content at WP [mm]
            wat%layer(2)%h_fc=1000*theta(2)%fc%mat*d_t            ! water soil content at FC [mm]
            wat%layer(2)%h_sat=1000*theta(2)%sat%mat*d_t          ! water soil content at saturation [mm] %AB%
            wat%layer(2)%h_r=1000*theta(2)%r%mat*d_t              ! water soil content at residual humidity [mm]
        end where
    end where
end subroutine update_soil_pars

subroutine update_irrigation_thresholds(bal, wat, p_day, r_stress, alpha_ms, alpha_unm, domain, mode)
    type(balance_matrices), intent(inout) :: bal
    type(wat_matrix), intent(in) :: wat
    real(dp), dimension(:,:), intent(in) :: p_day, r_stress, alpha_ms, alpha_unm
    type(grid_i), intent(in) :: domain
    integer, intent(in) :: mode
    real(dp), dimension(size(p_day, 1), size(p_day, 2)) :: h_fc, taw

    if (mode == 0) return

    h_fc = wat%layer(1)%h_fc + wat%layer(2)%h_fc
    taw = (wat%layer(1)%h_fc - wat%layer(1)%h_wp) + (wat%layer(2)%h_fc - wat%layer(2)%h_wp) ! Total available water

    where(domain%mat /= domain%header%nan)
        bal%h_irr_thresh = h_fc - taw * p_day * (alpha_ms + r_stress)
    end where

    if (mode == 1) then
        where(domain%mat /= domain%header%nan)
            bal%h_raw = h_fc - taw * p_day !%PS%: the "raw" naming here is a bit confusing. Read it as "profile water storage when RAW is fully depleted".
            bal%h_priv_thresh = h_fc - taw * p_day * (alpha_unm + r_stress)
        end where
    end if
end subroutine update_irrigation_thresholds

subroutine init_balance_matrices(bal, imax, jmax, f_allocate)
    type(balance_matrices), intent(inout) :: bal
    integer, intent(in) :: imax, jmax
    logical, intent(in) :: f_allocate
    integer :: layer_idx

    if (f_allocate) then
        do layer_idx=1,2
            allocate(bal%lyr(layer_idx)%thickness(imax,jmax), bal%lyr(layer_idx)%h_soil(imax,jmax),                &
                   & bal%lyr(layer_idx)%t_soil(imax,jmax), bal%lyr(layer_idx)%h_transp_act(imax,jmax),             &
                   & bal%lyr(layer_idx)%h_transp_pot(imax,jmax), bal%lyr(layer_idx)%h_perc(imax,jmax), source=0._dp)
        end do
        allocate(bal%h_eva(imax,jmax), bal%h_eva_pot(imax,jmax), bal%h_interc(imax,jmax), bal%h_pond(imax,jmax),          &
               & bal%h_inf(imax,jmax), bal%h_runoff(imax,jmax), bal%h_eff_rain(imax,jmax), bal%h_net_av_water(imax,jmax), &
               & bal%h_gross_av_water(imax,jmax), bal%k_s(imax,jmax), bal%depth_under_rz(imax,jmax),                      &
               & bal%h_caprise(imax,jmax), bal%h_rise(imax,jmax), bal%h_raw(imax,jmax),                                   &
               & bal%h_irr_thresh(imax,jmax), bal%h_priv_thresh(imax,jmax), source=0._dp                                  )
    else
        do layer_idx=1,2
            deallocate(bal%lyr(layer_idx)%thickness, bal%lyr(layer_idx)%h_soil, bal%lyr(layer_idx)%t_soil,        &
                     & bal%lyr(layer_idx)%h_transp_act, bal%lyr(layer_idx)%h_transp_pot, bal%lyr(layer_idx)%h_perc)
        end do
        deallocate(bal%h_eva, bal%h_eva_pot, bal%h_interc, bal%h_pond, bal%h_inf, bal%h_runoff,           &
                 & bal%h_eff_rain, bal%h_net_av_water, bal%h_gross_av_water, bal%k_s, bal%depth_under_rz, &
                 & bal%h_caprise, bal%h_rise, bal%h_raw, bal%h_irr_thresh, bal%h_priv_thresh              )
    end if
end subroutine init_balance_matrices

subroutine init_wat_bal_hour(wat_bal1_hour,imax,jmax,f_allocate)
    ! init/destroy wat_bal_hourly
   type(hourly),intent(inout)::wat_bal1_hour
    integer,intent(in)::imax
    integer,intent(in)::jmax
    logical,intent(in)::f_allocate
    integer::checkstat
    character (len=*),parameter:: error_message = "wat_bal1_hour has been wrongly allocated"

    if(f_allocate .eqv. .true.)then
        allocate(wat_bal1_hour%esten%k_e           (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%esten%k_r           (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message ! %RR% added
        allocate(wat_bal1_hour%esten%h_eva         (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%esten%h_eva_pot     (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%esten%h_inf         (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%esten%h_perc1       (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%esten%h_perc2      (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%esten%h_pond        (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%esten%h_transp_act1  (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%esten%h_transp_pot1  (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%esten%h_transp_act2 (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%esten%h_transp_pot2 (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%esten%h_caprise     (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%esten%h_rise        (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%esten%h_net_av_water (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%esten%h_gross_av_water (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%esten%h_eff_rain    (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%inten%h_soil1        (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%inten%h_soil2        (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%inten%k_s_dry        (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%inten%k_s_sat        (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%inten%k_s          (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%inten%h_pond0       (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%n_iter1            (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%n_iter2            (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%n_max1              (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
        allocate(wat_bal1_hour%n_max2              (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,error_message
    else
        deallocate(wat_bal1_hour%esten%k_e      )
        deallocate(wat_bal1_hour%esten%k_r      )! %RR% added
        deallocate(wat_bal1_hour%esten%h_eva      )
        deallocate(wat_bal1_hour%esten%h_eva_pot      )
        deallocate(wat_bal1_hour%esten%h_inf      )
        deallocate(wat_bal1_hour%esten%h_perc1    )
        deallocate(wat_bal1_hour%esten%h_perc2   )
        deallocate(wat_bal1_hour%esten%h_pond   )
        deallocate(wat_bal1_hour%esten%h_transp_act1    )
        deallocate(wat_bal1_hour%esten%h_transp_pot1  )
        deallocate(wat_bal1_hour%esten%h_transp_act2    )
        deallocate(wat_bal1_hour%esten%h_transp_pot2  )
        deallocate(wat_bal1_hour%esten%h_caprise  )
        deallocate(wat_bal1_hour%esten%h_rise  )
        deallocate(wat_bal1_hour%esten%h_net_av_water )
        deallocate(wat_bal1_hour%esten%h_gross_av_water)
        deallocate(wat_bal1_hour%esten%h_eff_rain     )
        deallocate(wat_bal1_hour%inten%h_soil1)
        deallocate(wat_bal1_hour%inten%h_soil2)
        deallocate(wat_bal1_hour%inten%k_s_dry  )
        deallocate(wat_bal1_hour%inten%k_s_sat  )
        deallocate(wat_bal1_hour%inten%k_s  )
        deallocate(wat_bal1_hour%inten%h_pond0   )
        deallocate(wat_bal1_hour%n_iter1)
        deallocate(wat_bal1_hour%n_iter2)
        deallocate(wat_bal1_hour%n_max1)
        deallocate(wat_bal1_hour%n_max2)
    end if
end subroutine init_wat_bal_hour

subroutine init_meteo_matrices(meteo,imax,jmax,f_allocate)
    ! init/destroy weather map
   type(meteo_mat),intent(inout)::meteo
    integer,intent(in)::imax
    integer,intent(in)::jmax
    logical,intent(in)::f_allocate
    integer::checkstat
    character (len=*),parameter:: errormessage = "meteo has been wrongly allocated"

    if(f_allocate)then
        allocate(meteo%T_max    (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,errormessage
        allocate(meteo%T_min    (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,errormessage
        allocate(meteo%P       (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,errormessage
        allocate(meteo%P_cum    (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,errormessage
        allocate(meteo%RH_max    (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,errormessage
        allocate(meteo%RH_min    (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,errormessage
        allocate(meteo%Wind_vel(imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,errormessage
        allocate(meteo%Rad_sol      (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,errormessage
        allocate(meteo%lat     (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,errormessage
        allocate(meteo%alt     (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,errormessage
        allocate(meteo%et0     (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,errormessage
        allocate(meteo%T_ave    (imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,errormessage
    else
        deallocate(meteo%T_max    )
        deallocate(meteo%T_min    )
        deallocate(meteo%P       )
        deallocate(meteo%P_cum   )
        deallocate(meteo%RH_max    )
        deallocate(meteo%RH_min    )
        deallocate(meteo%Wind_vel)
        deallocate(meteo%Rad_sol      )
        deallocate(meteo%lat     )
        deallocate(meteo%alt     )
        deallocate(meteo%et0     )
        deallocate(meteo%T_ave    )
    end if
end subroutine init_meteo_matrices

subroutine init_pheno_matrices(pheno,imax,jmax,f_allocate,crops)
    ! init/destroy pheno
   type(crop_pars_matrices),intent(inout)::pheno
    integer,intent(in)::imax
    integer,intent(in)::jmax
    logical,intent(in)::f_allocate
    type(crop_definition), optional, intent(in) :: crops(:)
    integer::checkstat, crop_id, max_curve_slots
    character (len=*),parameter:: errormessage = "pheno has been wrongly allocated"

    if(f_allocate)then
        allocate(pheno%crop_id           (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%sowing_year       (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%sowing_doy        (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%cuts_completed    (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%rotation_position (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%rotation_land_use (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%bare_soil_days_left(imax,jmax),stat=checkstat);if(checkstat/=0)print*,errormessage
        allocate(pheno%harvest_pending   (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%is_real_crop      (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%gdd               (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%vern_days         (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%crop_age_days     (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        max_curve_slots = 0
        do crop_id = 1, size(crops)
            if (.not. allocated(crops(crop_id)%gdd)) cycle
            max_curve_slots = max(max_curve_slots, size(crops(crop_id)%gdd)*crops(crop_id)%max_cuts)
        end do
        allocate(pheno%curve_day(imax,jmax,max_curve_slots),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%k_cb_old          (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%k_cb              (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%h                 (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%d_r               (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%lai               (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%cn_day            (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
         allocate(pheno%f_c              (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%irrigation_class  (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%cn_class          (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%p                 (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%a                 (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%d_t_max           (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%RF_t_max          (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%RF_e              (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%RF_t              (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%p_day             (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%pheno_stage       (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage
        allocate(pheno%r_stress          (imax,jmax),stat=checkstat); if(checkstat/=0)print*,errormessage

        pheno%crop_id = 0
        pheno%sowing_year = nan_i
        pheno%sowing_doy = nan_i
        pheno%cuts_completed = 0
        pheno%rotation_position = 1
        pheno%rotation_land_use = 0
        pheno%bare_soil_days_left = 0
        pheno%harvest_pending = .false.
        pheno%is_real_crop = .false.
        pheno%gdd = 0._dp
        pheno%vern_days = 0._dp
        pheno%crop_age_days = 0
        pheno%curve_day = huge(0)
        pheno%k_cb_old = 0._dp
        pheno%k_cb = 0._dp
        pheno%h = 0._dp
        pheno%d_r = 0._dp
        pheno%lai = 0._dp
        pheno%cn_day = 0
        pheno%f_c = 0._dp
        pheno%irrigation_class = 0
        pheno%cn_class = 1
        pheno%p = 0._dp
        pheno%a = 0._dp
        pheno%d_t_max = 0._dp
        pheno%RF_t_max = 0._dp
        pheno%RF_e = 0._dp
        pheno%RF_t = 0._dp
        pheno%p_day = 0._dp
        pheno%pheno_stage = 0
        pheno%r_stress = 0._dp

    else
        deallocate(pheno%crop_id)
        deallocate(pheno%sowing_year)
        deallocate(pheno%sowing_doy)
        deallocate(pheno%cuts_completed)
        deallocate(pheno%rotation_position)
        deallocate(pheno%rotation_land_use)
        deallocate(pheno%bare_soil_days_left)
        deallocate(pheno%harvest_pending)
        deallocate(pheno%is_real_crop)
        deallocate(pheno%gdd)
        deallocate(pheno%vern_days)
        deallocate(pheno%crop_age_days)
        if (allocated(pheno%curve_day)) deallocate(pheno%curve_day)
        deallocate(pheno%k_cb_old        )
        deallocate(pheno%k_cb            )
        deallocate(pheno%h              )
        deallocate(pheno%d_r             )
        deallocate(pheno%lai            )
        deallocate(pheno%cn_day         )
        deallocate(pheno%f_c            )
        deallocate(pheno%irrigation_class )
        deallocate(pheno%cn_class             )
        deallocate(pheno%p              )
        deallocate(pheno%a              )
        deallocate(pheno%d_t_max          )
        deallocate(pheno%RF_t_max         )
        deallocate(pheno%RF_e            )
        deallocate(pheno%RF_t            )
        deallocate(pheno%p_day           )
        deallocate(pheno%pheno_stage   )
        deallocate(pheno%r_stress   )
    end if
end subroutine init_pheno_matrices

subroutine init_wat_matrices(wat,imax,jmax,allocazione)
    ! init/destroy water matrices
   type(wat_matrix),intent(inout)::wat
    integer,intent(in)::imax
    integer,intent(in)::jmax
    logical,intent(in)::allocazione
    integer::checkstat
    character (len=50):: errormessage = "wat has been wrongly allocated"

    if(allocazione)then
        allocate(wat%layer(1)%h_wp(imax,jmax),stat=checkstat)   ; if(checkstat/=0)print*,errormessage
        allocate(wat%layer(1)%h_fc(imax,jmax),stat=checkstat)   ; if(checkstat/=0)print*,errormessage
        allocate(wat%layer(1)%h_sat(imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,errormessage
        allocate(wat%layer(1)%h_r(imax,jmax),stat=checkstat)    ; if(checkstat/=0)print*,errormessage
        allocate(wat%layer(1)%rew(imax,jmax),stat=checkstat)  ; if(checkstat/=0)print*,errormessage
        allocate(wat%layer(2)%h_wp(imax,jmax),stat=checkstat)   ; if(checkstat/=0)print*,errormessage
        allocate(wat%layer(2)%h_fc(imax,jmax),stat=checkstat)   ; if(checkstat/=0)print*,errormessage
        allocate(wat%layer(2)%h_sat(imax,jmax),stat=checkstat) ; if(checkstat/=0)print*,errormessage
        allocate(wat%layer(2)%h_r(imax,jmax),stat=checkstat)    ; if(checkstat/=0)print*,errormessage
        allocate(wat%theta2_rice(1)%h_wp(imax,jmax),stat=checkstat)    ; if(checkstat/=0)print*,errormessage
        allocate(wat%theta2_rice(1)%h_fc(imax,jmax),stat=checkstat)    ; if(checkstat/=0)print*,errormessage
        allocate(wat%theta2_rice(1)%h_sat(imax,jmax),stat=checkstat)  ; if(checkstat/=0)print*,errormessage
        allocate(wat%theta2_rice(1)%h_r(imax,jmax),stat=checkstat)     ; if(checkstat/=0)print*,errormessage
        allocate(wat%wat1_rew(imax,jmax),stat=checkstat)       ; if(checkstat/=0)print*,errormessage
        allocate(wat%few(imax,jmax),stat=checkstat)            ; if(checkstat/=0)print*,errormessage
        allocate(wat%kc_max(imax,jmax),stat=checkstat)          ; if(checkstat/=0)print*,errormessage
    else
        deallocate(wat%layer(1)%h_wp)
        deallocate(wat%layer(1)%h_fc)
        deallocate(wat%layer(1)%h_sat)
        deallocate(wat%layer(1)%h_r)
        deallocate(wat%layer(1)%rew)
        deallocate(wat%layer(2)%h_wp)
        deallocate(wat%layer(2)%h_fc)
        deallocate(wat%layer(2)%h_sat)
        deallocate(wat%layer(2)%h_r)
        deallocate(wat%theta2_rice(1)%h_wp)
        deallocate(wat%theta2_rice(1)%h_fc)
        deallocate(wat%theta2_rice(1)%h_sat)
        deallocate(wat%theta2_rice(1)%h_r)
        deallocate(wat%wat1_rew)
        deallocate(wat%few)
        deallocate(wat%kc_max)
    end if
end subroutine init_wat_matrices

end module cli_simulation_manager
