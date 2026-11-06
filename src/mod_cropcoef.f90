module mod_cropcoef

use mod_constants, only: dp, pi, nan_r
use mod_crop_phenology, only: crop_definition, crop_rotation, crop_pars_matrices
use mod_evapotranspiration, only: calculateDLH
use mod_grid, only: grid_i
use mod_meteo, only: meteo_mat
use mod_meteo, only: meteo_info
use mod_parameters, only: simulation
use mod_date, only: date, days_between_dates, days_in_year, month_lengths, calendar_date_from_doy, advance_calendar_date, &
                  & dates_are_equal

implicit none
private
public :: advance_crops_daily, adjust_water_productivity, compute_canopy_resistance, prepare_crop_stage_boundaries, &
        & compute_kcb_corrections, crop_weather_series, load_crop_weather

type crop_weather_series
    type(date) :: start
    type(date) :: replay_start, replay_end
    real(dp), allocatable :: tmax(:), tmin(:)
end type crop_weather_series

contains

! Load into memory full station-level temperature series to be used in GDD calculations
! todo: the resulting array is rather big. We should consider alternatives (e.g. loading smaller chunks at a time)
subroutine load_crop_weather(weather, stations, path, replay_start, replay_end)
    type(crop_weather_series), allocatable, intent(out) :: weather(:)
    type(meteo_info), intent(in) :: stations(:)
    character(len=*), intent(in) :: path
    type(date), optional, intent(in) :: replay_start, replay_end

    integer :: station, day, unit, ios, header_line, n_days, source_days, first_replay_row, last_replay_row
    real(dp) :: unused_variables(5)
    character(len=300) :: header

    allocate(weather(size(stations)))
    do station = 1, size(stations)
        weather(station)%start = stations(station)%start
        source_days = days_between_dates(stations(station)%start, stations(station)%finish) + 1
        n_days = source_days
        if (present(replay_end)) then
            ! Warm-up runs once through the input year, then the simulation replays it from replay_start.
            ! Project its surviving crops against that same repeated weather, followed by later input years.
            first_replay_row = days_between_dates(stations(station)%start, replay_start) + 1
            last_replay_row = days_between_dates(stations(station)%start, replay_end) + 1
            n_days = last_replay_row + (source_days - first_replay_row + 1)
            weather(station)%replay_start = replay_start
            weather(station)%replay_end = replay_end
        end if
        allocate(weather(station)%tmax(n_days), weather(station)%tmin(n_days))
        open(newunit=unit, file=trim(path)//trim(stations(station)%filename), status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Cannot reopen meteorological series for crop projection: ', trim(stations(station)%filename)
            stop
        end if
        do header_line = 1, 4
            read(unit, '(A)', iostat=ios) header
            if (ios /= 0) exit
        end do
        do day = 1, source_days
            if (ios /= 0) exit
            read(unit, *, iostat=ios) weather(station)%tmax(day), weather(station)%tmin(day), unused_variables
        end do
        close(unit)
        if (ios /= 0) then
            print *, 'Incomplete meteorological series for crop projection: ', trim(stations(station)%filename)
            stop
        end if
        if (present(replay_end)) then
            weather(station)%tmax(last_replay_row+1:) = weather(station)%tmax(first_replay_row:source_days)
            weather(station)%tmin(last_replay_row+1:) = weather(station)%tmin(first_replay_row:source_days)
        end if
    end do
end subroutine load_crop_weather

subroutine compute_kcb_corrections(kcb_corr_fact, crops, stations, sim)
    real(dp), allocatable :: kcb_corr_fact(:,:)
    type(crop_definition), intent(in) :: crops(:)
    type(meteo_info), intent(in) :: stations(:)
    type(simulation), intent(in) :: sim

    real(dp), allocatable :: rhmin(:), wind(:)
    real(dp) :: tmax, tmin, rain, rhmax, radiation, mean_rhmin, mean_wind
    integer :: crop_idx, station_idx, n_days, day_idx, n_years, partial_days, unit, ios, header_line
    character(len=300) :: header

    allocate(kcb_corr_fact(size(crops),size(stations)), source=0._dp)

    do crop_idx = 1, size(crops)
        if (.not. needs_kcb_adjustment(crops(crop_idx))) cycle
        if ((crops(crop_idx)%kcb_corr_start_month == 0) .neqv. (crops(crop_idx)%kcb_corr_end_month == 0)) then
            print *, 'Both KcbCorrectionStart and KcbCorrectionEnd are required in ', &
                & trim(crops(crop_idx)%parameter_file), '.'
            print *, 'Execution will be aborted...'
            stop
        end if
        if (crops(crop_idx)%kcb_corr_start_month == 0) then
            print *, 'Warning: ', trim(crops(crop_idx)%parameter_file), ' has no Kcb correction interval.'
            print *, 'Using 70-95% of the estimated sowing-to-harvest period for climate averages.'
        end if
    end do

    do station_idx = 1, size(stations)
        n_days = days_between_dates(stations(station_idx)%start, stations(station_idx)%finish) + 1
        allocate(rhmin(n_days), wind(n_days))
        open(newunit=unit, file=trim(sim%meteo_path)//trim(stations(station_idx)%filename), &
             & status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Cannot reopen meteorological series for Kcb correction: ', trim(stations(station_idx)%filename)
            print *, 'Execution will be aborted...'
            stop
        end if
        do header_line = 1, 4
            read(unit, '(A)', iostat=ios) header
            if (ios /= 0) exit
        end do
        do day_idx = 1, n_days
            if (ios /= 0) exit
            read(unit, *, iostat=ios) tmax, tmin, rain, rhmax, rhmin(day_idx), wind(day_idx), radiation ! Only store rhmin and wind
        end do
        close(unit)
        if (ios /= 0) then
            print *, 'Incomplete meteorological series for Kcb correction: ', trim(stations(station_idx)%filename)
            print *, 'Execution will be aborted...'
            stop
        end if

        do crop_idx = 1, size(crops)
            if (.not. needs_kcb_adjustment(crops(crop_idx))) cycle

            ! Get average RHmin and wind speed over the crop's Kcb correction interval
            call mean_correction_weather(crops(crop_idx), stations(station_idx), rhmin, wind, &
                                       & mean_rhmin, mean_wind, n_years, partial_days         )
            if (n_years == 0) then
                if (partial_days > 0) then
                    print *, 'Warning: no complete Kcb correction interval for ', trim(crops(crop_idx)%crop_name), &
                        & ' at weather station ', trim(stations(station_idx)%filename), '.'
                    print *, 'Used ', partial_days, ' available day(s) within incomplete interval(s).'
                else
                    print *, 'Warning: no Kcb correction interval overlaps weather station ', &
                        & trim(stations(station_idx)%filename), ' for ', trim(crops(crop_idx)%crop_name), '.'
                    print *, 'Used the entire available weather series instead.'
                end if
            end if

            ! Calculate the correction factor
            !%PS%, note: FAO56 prescribe using the mean plant height during mid-late season; here we use max height for simplicity
            kcb_corr_fact(crop_idx,station_idx) = k_cb_weather_correction(mean_rhmin, mean_wind, maxval(crops(crop_idx)%height))
        end do
        deallocate(rhmin, wind)
    end do
end subroutine compute_kcb_corrections

function needs_kcb_adjustment(crop) result(is_eligible)
    type(crop_definition), intent(in) :: crop
    logical :: is_eligible

    is_eligible = .false.
    if (.not. crop%adjust_k_cb) return
    if (.not. allocated(crop%k_cb)) return
    if (size(crop%k_cb) == 0) return
    is_eligible = crop%mid_start_gdd < huge(0._dp) .and. crop%mid_end_gdd > crop%mid_start_gdd .and. maxval(crop%k_cb) > 0.45_dp
end function needs_kcb_adjustment

! Calculate mean RHmin and wind speed over the crop's Kcb correction interval
subroutine mean_correction_weather(crop, station, rhmin, wind, mean_rhmin, mean_wind, n_years, partial_days)
    type(crop_definition), intent(in) :: crop
    type(meteo_info), intent(in) :: station
    real(dp), intent(in) :: rhmin(:), wind(:)
    real(dp), intent(out) :: mean_rhmin, mean_wind
    integer, intent(out) :: n_years, partial_days

    type(date) :: first_day, last_day
    real(dp) :: partial_rhmin, partial_wind
    integer :: year, first_idx, last_idx, n_days, start_key, end_key, days_in_month(12)
    integer :: sowing_idx, harvest_idx, harvest_year, estimated_sowing_doy, growing_days

    mean_rhmin = 0._dp
    mean_wind = 0._dp
    n_years = 0
    partial_rhmin = 0._dp
    partial_wind = 0._dp
    partial_days = 0
    start_key = 100*crop%kcb_corr_start_month + crop%kcb_corr_start_day
    end_key = 100*crop%kcb_corr_end_month + crop%kcb_corr_end_day

    do year = station%start%year - 1, station%finish%year
        if (crop%kcb_corr_start_month == 0) then
            ! Crop file does not specify a Kcb correction interval; use 70-95% of the estimated sowing-to-harvest period
            estimated_sowing_doy = max(1, crop%sowing_doy_min + max(0, crop%sowing_delay_max)/2)
            sowing_idx = days_between_dates(station%start, calendar_date_from_doy(year, 1)) + estimated_sowing_doy
            harvest_year = year + merge(1, 0, crop%harvest_doy_max <= estimated_sowing_doy)
            harvest_idx = days_between_dates(station%start, calendar_date_from_doy(harvest_year, 1)) + &
                & min(days_in_year(harvest_year), max(1, crop%harvest_doy_max))
            growing_days = harvest_idx - sowing_idx
            if (growing_days <= 0) cycle
            first_idx = sowing_idx + nint(0.70_dp*real(growing_days,dp))
            last_idx = sowing_idx + nint(0.95_dp*real(growing_days,dp))
        else
            first_day%day = crop%kcb_corr_start_day
            first_day%month = crop%kcb_corr_start_month
            first_day%year = year
            last_day%day = crop%kcb_corr_end_day
            last_day%month = crop%kcb_corr_end_month
            last_day%year = year + merge(1, 0, start_key > end_key)
            days_in_month = month_lengths(first_day%year)
            if (first_day%day > days_in_month(first_day%month)) cycle
            days_in_month = month_lengths(last_day%year)
            if (last_day%day > days_in_month(last_day%month)) cycle
            first_idx = days_between_dates(station%start, first_day) + 1
            last_idx = days_between_dates(station%start, last_day) + 1
        end if
        if (last_idx < first_idx) cycle
        if (first_idx < 1 .or. last_idx > size(rhmin)) then
            first_idx = max(1, first_idx)
            last_idx = min(size(rhmin), last_idx)
            if (last_idx >= first_idx) then
                partial_rhmin = partial_rhmin + sum(rhmin(first_idx:last_idx))
                partial_wind = partial_wind + sum(wind(first_idx:last_idx))
                partial_days = partial_days + last_idx - first_idx + 1
            end if
            cycle
        end if
        n_days = last_idx - first_idx + 1
        mean_rhmin = mean_rhmin + sum(rhmin(first_idx:last_idx))/real(n_days,dp)
        mean_wind = mean_wind + sum(wind(first_idx:last_idx))/real(n_days,dp)
        n_years = n_years + 1
    end do
    if (n_years > 0) then
        mean_rhmin = mean_rhmin/real(n_years,dp)
        mean_wind = mean_wind/real(n_years,dp)
    else if (partial_days > 0) then
        mean_rhmin = partial_rhmin/real(partial_days,dp)
        mean_wind = partial_wind/real(partial_days,dp)
    else
        mean_rhmin = sum(rhmin)/real(size(rhmin),dp)
        mean_wind = sum(wind)/real(size(wind),dp)
    end if
end subroutine mean_correction_weather

! Same clamped FAO-56 correction used by CropCoef's calcKcbCorrFact().
real(dp) function k_cb_weather_correction(rhmin, wind, height) result(correction)
    real(dp), intent(in) :: rhmin, wind, height

    correction = (0.04_dp*(min(6._dp,max(1._dp,wind)) - 2._dp) - &
        & 0.004_dp*(min(80._dp,max(20._dp,rhmin)) - 45._dp)) * &
        & (min(10._dp,max(0.1_dp,height))/3._dp)**0.3_dp
end function k_cb_weather_correction

! CropCoef's CO2 correction of normalized biomass water productivity.
real(dp) function adjust_water_productivity(orig_wp, co2, sink_strength) result(adjusted_wp)
    real(dp), intent(in) :: orig_wp, co2, sink_strength
    real(dp) :: crop_type, weight, co2_factor

    crop_type = min(1._dp, max(0._dp, (40._dp - orig_wp*100._dp)/20._dp))
    weight = min(1._dp, max(0._dp, 1._dp - (550._dp - co2)/(550._dp - 369.41_dp)))
    co2_factor = (co2/369.41_dp) / (1._dp + (co2 - 369.41_dp) * &
        & ((1._dp - weight)*0.000138_dp + weight*(sink_strength*0.000138_dp + &
        & (1._dp - sink_strength)*0.001165_dp)))
    adjusted_wp = (1._dp + crop_type*(co2_factor - 1._dp))*orig_wp
end function adjust_water_productivity

! CropCoef's annual canopy-resistance correction (EPIC/FAO56).
real(dp) function compute_canopy_resistance(co2) result(resistance)
    real(dp), intent(in) :: co2
    real(dp) :: leaf_conductance

    leaf_conductance = 0.01_dp * (1.4_dp - 0.4_dp*(co2/330._dp))
    resistance = (1._dp/leaf_conductance)/(0.5_dp*24._dp*0.12_dp)
end function compute_canopy_resistance

subroutine advance_crops_daily(pheno, crops, bare_soil, rotations, domain, soil_use, read_crop_maps, &
                             & crop_map, irandom, meteo, kcb_corr_fact, station_idx, station_weight, &
                             & interpolate_temp, weather, current, aggressive_sowing                 )
    type(crop_pars_matrices), intent(inout) :: pheno
    type(crop_definition), intent(in) :: crops(:)
    type(crop_definition), intent(in) :: bare_soil
    type(crop_rotation), intent(inout) :: rotations(:)
    type(grid_i), intent(in) :: domain, soil_use, crop_map
    logical, intent(in) :: read_crop_maps
    integer, intent(in) :: irandom(:,:)
    type(meteo_mat), intent(in) :: meteo
    real(dp), intent(in) :: kcb_corr_fact(:,:)
    integer, intent(in) :: station_idx(:,:,:)
    real(dp), intent(in) :: station_weight(:,:,:)
    logical, intent(in) :: interpolate_temp
    logical, intent(in) :: aggressive_sowing
    type(crop_weather_series), intent(in) :: weather(:)
    type(date), intent(in) :: current

    integer :: i, j, land_use, position, crop_id, prescribed_crop_id
    real(dp) :: temperature, cut_gdd

    pheno%k_cb_old = pheno%k_cb

    do j = 1, size(domain%mat, 2)
        do i = 1, size(domain%mat, 1)
            if (domain%mat(i,j) == domain%header%nan) cycle ! Skip inactive cells

            land_use = soil_use%mat(i,j)

            if (read_crop_maps) then ! Sow/harvest using maps, skipping usual logic (crops are only sown/harvested when the map changes)
                prescribed_crop_id = crop_map%mat(i,j)
                if (pheno%harvest_pending(i,j)) call set_baresoil(pheno, i, j)

                if (pheno%crop_id(i,j) == 0) then
                    call set_bare_soil_today(pheno, bare_soil, i, j)
                    if (prescribed_crop_id == 0) cycle
                    crop_id = prescribed_crop_id
                    call sow_crop(pheno, crops(crop_id), crop_id, i, j, current, meteo%lat(i,j),      &
                                & station_idx(i,j,:), station_weight(i,j,:), interpolate_temp, weather)
                else
                    crop_id = pheno%crop_id(i,j)
                end if
            else
                if (pheno%harvest_pending(i,j)) call harvest_crop(pheno, crops, rotations(land_use),   &
                                                                & land_use, i, j, current, irandom(i,j))

                if (pheno%crop_id(i,j) == 0) then ! Soil is bare: consider sowing
                    call set_bare_soil_today(pheno, bare_soil, i, j)
                    position = pheno%rotation_position(i,j)

                    ! First baresoil day after a landuse change: find the earliest sowable crop
                    if (pheno%rotation_land_use(i,j) /= land_use) then
                        position = next_sowing_position(rotations(land_use), crops, current, irandom(i,j))
                        pheno%rotation_land_use(i,j) = land_use
                    end if

                    ! Warn if a crop's sowing window passes without sowing (due to overlapping crops or bad temperatures)
                    call skip_missed_sowing_windows(position, rotations(land_use), crops, current, irandom(i,j), land_use)
                    pheno%rotation_position(i,j) = position

                    ! Count down bare soil period requirement
                    if (pheno%bare_soil_days_left(i,j) > 0) then
                        pheno%bare_soil_days_left(i,j) = pheno%bare_soil_days_left(i,j) - 1
                        cycle ! soil remains bare - go to next cell
                    end if

                    ! Sow the next crop in the rotation if its sowing window is open and temperature requirements are met
                    crop_id = rotations(land_use)%crop_ids(position)
                    temperature = (meteo%T_max(i,j) + meteo%T_min(i,j))/2._dp
                    if (.not. should_sow_today(crops(crop_id), current%doy, irandom(i,j), temperature)) cycle
                    call sow_crop(pheno, crops(crop_id), crop_id, i, j, current, meteo%lat(i,j),      &
                                & station_idx(i,j,:), station_weight(i,j,:), interpolate_temp, weather)
                else ! Continue the crop already in field
                    crop_id = pheno%crop_id(i,j)
                end if
            end if

            ! Crop is in field: calculate GDD accumulation
            call advance_crop_GDD(crops(crop_id), meteo%T_max(i,j), meteo%T_min(i,j), current%doy, &
                                & meteo%lat(i,j), pheno%gdd(i,j), pheno%vern_days(i,j)             )
            pheno%crop_age_days(i,j) = pheno%crop_age_days(i,j) + 1

            ! For multi-cut crops, increase the cut counter if the accumulated GDD has reached the next cut threshold
            cut_gdd = maxval(crops(crop_id)%gdd)
            call update_completed_cuts(pheno, crops(crop_id), cut_gdd, i, j)

            ! Use the new GDD value to estimate crop state
            call populate_crop_today(pheno, crops(crop_id), kcb_corr_fact(crop_id,station_idx(i,j,1)), i, j)

            if (.not. read_crop_maps) then
                pheno%harvest_pending(i,j) = should_harvest_crop(pheno, crops, rotations(land_use), land_use, cut_gdd, &
                                                               & i, j, current, irandom(i,j), aggressive_sowing        )
            end if
        end do
    end do
end subroutine advance_crops_daily

! Harvest the crop (becoming bare soil), then advance its rotation position
subroutine harvest_crop(pheno, crops, rotation, land_use, i, j, current_date, irandom)
    type(crop_pars_matrices), intent(inout) :: pheno
    type(crop_definition), intent(in) :: crops(:)
    type(crop_rotation), intent(in) :: rotation
    type(date), intent(in) :: current_date
    integer, intent(in) :: land_use, i, j, irandom
    integer :: position, crop_id

    call set_baresoil(pheno, i, j)

    ! Move to next crop in the rotation
    if (pheno%rotation_land_use(i,j) /= land_use) then ! A land-use change occurred; find the closest sowing window
        position = next_sowing_position(rotation, crops, current_date, irandom)
    else                                               ! Normal rotation advancement
        position = merge(1, pheno%rotation_position(i,j) + 1, pheno%rotation_position(i,j) == size(rotation%crop_ids))
    end if
    pheno%rotation_position(i,j) = position
    pheno%rotation_land_use(i,j) = land_use
    crop_id = rotation%crop_ids(position)
    pheno%bare_soil_days_left(i,j) = crops(crop_id)%crop_overlap_days
end subroutine harvest_crop

subroutine set_baresoil(pheno, i, j)
    type(crop_pars_matrices), intent(inout) :: pheno
    integer, intent(in) :: i, j

    pheno%harvest_pending(i,j) = .false.
    pheno%crop_id(i,j) = 0
    pheno%gdd(i,j) = 0._dp
    pheno%vern_days(i,j) = 0._dp
    pheno%cuts_completed(i,j) = 0
    pheno%pheno_stage(i,j) = 0
end subroutine set_baresoil

! Sow a new crop: initialize phenology and project the curve's GDD threshold dates.
subroutine sow_crop(pheno, crop, id, i, j, current, latitude, stations, weights, interpolate_temp, weather)
    type(crop_pars_matrices), intent(inout) :: pheno
    type(crop_definition), intent(in) :: crop
    integer, intent(in) :: id, i, j, stations(:)
    type(date), intent(in) :: current
    real(dp), intent(in) :: latitude, weights(:)
    logical, intent(in) :: interpolate_temp
    type(crop_weather_series), intent(in) :: weather(:)
    integer :: n_points

    pheno%crop_id(i,j) = id
    pheno%sowing_year(i,j) = current%year
    pheno%sowing_doy(i,j) = current%doy
    pheno%gdd(i,j) = 0._dp
    pheno%vern_days(i,j) = 0._dp
    pheno%cuts_completed(i,j) = 0
    pheno%pheno_stage(i,j) = 0
    pheno%is_real_crop(i,j) = maxval(crop%gdd) > 0._dp
    pheno%crop_age_days(i,j) = 0
    n_points = size(crop%gdd) * crop%max_cuts
    pheno%curve_day(i,j,1:n_points) = project_curve_days(crop, current, latitude, stations, weights, interpolate_temp, weather)
end subroutine sow_crop

subroutine update_completed_cuts(pheno, crop, cut_gdd, i, j)
    type(crop_pars_matrices), intent(inout) :: pheno
    type(crop_definition), intent(in) :: crop
    real(dp), intent(in) :: cut_gdd
    integer, intent(in) :: i, j

    if (crop%max_cuts <= 1) return
    do while (pheno%cuts_completed(i,j) < crop%max_cuts - 1)
        if (pheno%gdd(i,j) <= real(pheno%cuts_completed(i,j) + 1, dp) * cut_gdd) exit
        pheno%cuts_completed(i,j) = pheno%cuts_completed(i,j) + 1
    end do
end subroutine update_completed_cuts

! Mark harvest after today's growth, so the crop becomes bare soil tomorrow.
logical function should_harvest_crop(pheno, crops, rotation, land_use, cut_gdd, i, j, current, irandom, aggressive_sowing) &
                                   & result(harvest)
    type(crop_pars_matrices), intent(in) :: pheno
    type(crop_definition), intent(in) :: crops(:)
    type(crop_rotation), intent(in) :: rotation
    type(date), intent(in) :: current
    integer, intent(in) :: land_use, i, j, irandom
    logical, intent(in) :: aggressive_sowing
    real(dp), intent(in) :: cut_gdd
    integer :: id, next_position

    id = pheno%crop_id(i,j)

    ! Harvest when the final cut's GDD target or the latest harvest date is reached
    harvest = pheno%gdd(i,j) >= real(crops(id)%max_cuts, dp)*cut_gdd .or. &
            & (current%doy >= crops(id)%harvest_doy_max .and. &
            & (current%year > pheno%sowing_year(i,j) .or. crops(id)%harvest_doy_max > pheno%sowing_doy(i,j)))
    if (harvest) return

    ! Harvest to make room for the next crop's sowing window
    if (aggressive_sowing) then
        if (pheno%rotation_land_use(i,j) /= land_use) then
            next_position = next_sowing_position(rotation, crops, current, irandom)
        else
            next_position = merge(1, pheno%rotation_position(i,j) + 1, pheno%rotation_position(i,j) == size(rotation%crop_ids))
        end if
        id = rotation%crop_ids(next_position)
        harvest = should_clear_field(crops(id), current, pheno%sowing_year(i,j), pheno%sowing_doy(i,j), irandom)
    end if
end function should_harvest_crop

! If harvested after today, can the next crop be sown once its bare-soil interval has passed?
logical function should_clear_field(next_crop, current_date, sowing_year, sowing_doy, irandom) result(needs_field)
    type(crop_definition), intent(in) :: next_crop
    type(date), intent(in) :: current_date
    integer, intent(in) :: sowing_year, sowing_doy, irandom
    integer :: first_day, last_day, window_year, available_year, available_doy
    needs_field = .false.

    ! Calculate the sowing window for the next crop
    call sowing_window(next_crop, irandom, first_day, last_day)

    ! Earliest available day for sowing the next crop (tomorrow + crop_overlap_days)
    available_year = current_date%year
    available_doy = current_date%doy + next_crop%crop_overlap_days + 1
    do while (available_doy > days_in_year(available_year))
        available_doy = available_doy - days_in_year(available_year)
        available_year = available_year + 1
    end do

    ! Doesn't clear the field if the next crop's sowing window is in a different year
    window_year = current_date%year
    if (window_year == sowing_year .and. first_day <= sowing_doy) window_year = window_year + 1
    if (available_year /= window_year) return

    needs_field = available_doy >= first_day .and. available_doy <= last_day
end function should_clear_field

! After a land-use change, prefer the earliest-opened sowing window; otherwise choose the nearest upcoming one.
integer function next_sowing_position(rotation, crops, current, irandom) result(position)
    type(crop_rotation), intent(in) :: rotation
    type(crop_definition), intent(in) :: crops(:)
    type(date), intent(in) :: current
    integer, intent(in) :: irandom
    integer :: candidate, crop_id, first_day, last_day, wait_days, shortest_wait, earliest_start

    position = 1
    shortest_wait = huge(0)
    earliest_start = huge(0)
    do candidate = 1, size(rotation%crop_ids)
        crop_id = rotation%crop_ids(candidate)
        call sowing_window(crops(crop_id), irandom, first_day, last_day)

        if (current%doy <= last_day) then
            wait_days = max(0, first_day - current%doy)
        else
            wait_days = days_in_year(current%year) - current%doy + first_day
        end if
        if (wait_days > shortest_wait) cycle
        if (wait_days == shortest_wait .and. first_day >= earliest_start) cycle
        shortest_wait = wait_days
        earliest_start = first_day
        position = candidate
    end do
end function next_sowing_position

subroutine skip_missed_sowing_windows(position, rotation, crops, current, irandom, land_use)
    integer, intent(inout) :: position
    type(crop_rotation), intent(inout) :: rotation
    type(crop_definition), intent(in) :: crops(:)
    type(date), intent(in) :: current
    integer, intent(in) :: irandom, land_use
    integer :: considered, crop_id, first_day, last_day

    do considered = 1, size(rotation%crop_ids)
        crop_id = rotation%crop_ids(position)

        call sowing_window(crops(crop_id), irandom, first_day, last_day)
        if (current%doy <= last_day) return

        if (size(rotation%crop_ids) > 1 .and. .not. rotation%missed_sowing_warned) then
            print *, 'Warning: land use ', land_use, ' missed the sowing window for ', &
                     & trim(crops(crop_id)%crop_name), ' in ', current%year, ' (day ', current%doy, ').'
            print *, 'The rotation advances; further missed-sowing warnings for this land use are suppressed.'
            rotation%missed_sowing_warned = .true.
        end if

        position = merge(1, position + 1, position == size(rotation%crop_ids))
    end do
end subroutine skip_missed_sowing_windows

subroutine sowing_window(crop, irandom, first_day, last_day)
    type(crop_definition), intent(in) :: crop
    integer, intent(in) :: irandom
    integer, intent(out) :: first_day, last_day

    first_day = max(1, crop%sowing_doy_min + irandom)
    last_day = first_day + max(0, crop%sowing_delay_max)
end subroutine sowing_window

logical function should_sow_today(crop, doy, irandom, temperature)
    type(crop_definition), intent(in) :: crop
    integer, intent(in) :: doy, irandom
    real(dp), intent(in) :: temperature
    integer :: first_day, last_day

    call sowing_window(crop, irandom, first_day, last_day)

    should_sow_today = doy >= first_day .and. doy <= last_day .and. (temperature > crop%sowing_temp .or. doy == last_day)
end function should_sow_today

subroutine advance_crop_GDD(crop, tmax, tmin, doy, latitude, accumulated_gdd, vern_days)
    type(crop_definition), intent(in) :: crop
    real(dp), intent(in) :: tmax, tmin, latitude
    integer, intent(in) :: doy
    real(dp), intent(inout) :: accumulated_gdd, vern_days

    real(dp) :: daily_gdd, temperature, daylength, vern_factor, photo_factor

    daily_gdd = calculate_daily_gdd(tmax, tmin, crop%base_temp, crop%cutoff_temp)
    vern_factor = 1._dp
    photo_factor = 1._dp

    if (crop%requires_vernalization) then
        temperature = (tmax + tmin)/2._dp
        vern_factor = vernalization_factor(crop, temperature, vern_days)
    end if
    if (crop%photoperiod_response /= 0) then
        call calculateDLH(doy, latitude, daylength)
        photo_factor = photoperiod_factor(crop, daylength)
    end if

    accumulated_gdd = accumulated_gdd + daily_gdd * min(vern_factor, photo_factor)
end subroutine advance_crop_GDD

real(dp) function calculate_daily_gdd(tmax, tmin, base, cutoff) result(gdd)
    ! Single-day form of CropCoef's sine-wave calculateGDD() (Borghi, eq. i-85).
    real(dp), intent(in) :: tmax, tmin, base, cutoff
    real(dp) :: average, amplitude, lower_area, upper_area, theta, phi

    gdd = 0._dp
    if (tmax <= base .or. cutoff <= base) return

    average = (tmax + tmin)/2._dp
    amplitude = (tmax - tmin)/2._dp
    lower_area = 0._dp
    upper_area = 0._dp

    if (tmin >= cutoff) then
        gdd = cutoff - base
        return
    else if (tmin >= base .and. tmax <= cutoff) then
        lower_area = average - base
    else if (tmin < base .and. tmax <= cutoff) then
        theta = asin(max(-1._dp, min(1._dp, (base - average)/amplitude)))
        lower_area = ((average - base)*(pi/2._dp - theta) + amplitude*cos(theta))/pi
    else if (tmin >= base .and. tmax > cutoff) then
        phi = asin(max(-1._dp, min(1._dp, (cutoff - average)/amplitude)))
        lower_area = average - base
        upper_area = ((average - cutoff)*(pi/2._dp - phi) + amplitude*cos(phi))/pi
    else
        theta = asin(max(-1._dp, min(1._dp, (base - average)/amplitude)))
        phi = asin(max(-1._dp, min(1._dp, (cutoff - average)/amplitude)))
        lower_area = ((average - base)*(pi/2._dp - theta) + amplitude*cos(theta))/pi
        upper_area = ((average - cutoff)*(pi/2._dp - phi) + amplitude*cos(phi))/pi
    end if

    gdd = max(0._dp, lower_area - upper_area)
end function calculate_daily_gdd

real(dp) function vernalization_factor(crop, temperature, accumulated_days) result(factor)
    ! CropCoef's vernalization contribution and factor, evaluated incrementally.
    type(crop_definition), intent(in) :: crop
    real(dp), intent(in) :: temperature
    real(dp), intent(inout) :: accumulated_days
    real(dp) :: contribution

    factor = 1._dp
    if (.not. crop%requires_vernalization) return

    contribution = 0._dp
    if (crop%vern_curve_slope > 0._dp) then
        if (temperature >= crop%vern_temp_min - crop%vern_curve_slope .and. temperature < crop%vern_temp_min) then
            contribution = 1._dp - (crop%vern_temp_min - temperature)/crop%vern_curve_slope
        else if (temperature >= crop%vern_temp_max .and. temperature < crop%vern_temp_max + crop%vern_curve_slope) then
            contribution = 1._dp - (temperature - crop%vern_temp_max)/crop%vern_curve_slope
        end if
    end if
    if (temperature >= crop%vern_temp_min .and. temperature < crop%vern_temp_max) contribution = 1._dp
    accumulated_days = accumulated_days + contribution

    if (accumulated_days < crop%vern_days_start .or. accumulated_days > crop%vern_days_end) return
    if (crop%vern_days_end <= crop%vern_days_start) return

    factor = crop%vern_fact_min + (1._dp - crop%vern_fact_min) * &
           & (accumulated_days - crop%vern_days_start) / (crop%vern_days_end - crop%vern_days_start)
end function vernalization_factor

real(dp) function photoperiod_factor(crop, daylength) result(factor)
    ! Long-day and short-day responses from CropCoef's photoperiod().
    type(crop_definition), intent(in) :: crop
    real(dp), intent(in) :: daylength

    factor = 1._dp
    select case (crop%photoperiod_response)
    case (1)
        if (daylength < crop%daylength_if) then
            factor = 0._dp
        else if (daylength <= crop%daylength_ins .and. crop%daylength_ins > crop%daylength_if) then
            factor = (daylength - crop%daylength_if)/(crop%daylength_ins - crop%daylength_if)
        end if
    case (2)
        if (daylength > crop%daylength_if) then
            factor = 0._dp
        else if (daylength >= crop%daylength_ins .and. crop%daylength_if > crop%daylength_ins) then
            factor = (crop%daylength_if - daylength)/(crop%daylength_if - crop%daylength_ins)
        end if
    end select
end function photoperiod_factor

! Looks ahead at future weather and takes note of predicted crop age at tabulated GDD thresholds
! %PS%: this allow crop parameters to increase and decrease linearly in time, as in FAO56, without having to store the entire curves (the drawback is that we compute GDD accumulation twice for each crop)
function project_curve_days(crop, sowing_date, latitude, stations, weights, interpolate_temp, weather) result(curve_days)
    type(crop_definition), intent(in) :: crop
    type(date), intent(in) :: sowing_date
    real(dp), intent(in) :: latitude, weights(:)
    integer, intent(in) :: stations(:)
    logical, intent(in) :: interpolate_temp
    type(crop_weather_series), intent(in) :: weather(:)
    integer :: curve_days(size(crop%gdd)*crop%max_cuts)

    type(date) :: projected_date, weather_date
    integer :: first_row(size(stations)), day, stat, row, max_days, point, cut, n_points, n_cuts
    real(dp) :: tmax, tmin, accumulated_gdd, vern_days, target, cut_gdd
    logical :: replayed

    curve_days = huge(0)
    n_points = size(crop%gdd)
    n_cuts = crop%max_cuts
    cut_gdd = maxval(crop%gdd)
    if (cut_gdd <= 0._dp) return

    ! Make sure we're not projecting crop development beyond the end of the weather series
    max_days = huge(0)
    do stat = 1, merge(size(stations), 1, interpolate_temp)
        first_row(stat) = days_between_dates(weather(stations(stat))%start, sowing_date) + 1
        max_days = min(max_days, size(weather(stations(stat))%tmax) - first_row(stat) + 1)
    end do

    projected_date = sowing_date
    weather_date = sowing_date
    replayed = .false.
    accumulated_gdd = 0._dp
    vern_days = 0._dp
    point = 1
    cut = 0
    do day = 1, max_days
        tmax = 0._dp
        tmin = 0._dp
        if (interpolate_temp) then
            do stat = 1, size(stations)
                row = first_row(stat) + day - 1
                tmax = tmax + weather(stations(stat))%tmax(row)*weights(stat)
                tmin = tmin + weather(stations(stat))%tmin(row)*weights(stat)
            end do
        else
            row = first_row(1) + day - 1
            tmax = weather(stations(1))%tmax(row)
            tmin = weather(stations(1))%tmin(row)
        end if

        call advance_crop_GDD(crop, tmax, tmin, weather_date%doy, latitude, accumulated_gdd, vern_days)

        do
            target = crop%gdd(point) + real(cut,dp)*cut_gdd
            if (accumulated_gdd < target) exit
            curve_days(cut*n_points + point) = day
            point = point + 1
            if (point > n_points) then
                point = 1
                cut = cut + 1
                if (cut == n_cuts) return
            end if
        end do

        call advance_calendar_date(projected_date)
        if (.not. replayed .and. dates_are_equal(weather_date, weather(1)%replay_end)) then
            weather_date = weather(1)%replay_start ! Replay the same year if in warmup
            replayed = .true.
        else
            call advance_calendar_date(weather_date)
        end if
    end do
end function project_curve_days

subroutine curve_day_interval(days, age, left_point, right_point, fraction)
    integer, intent(in) :: days(:), age
    integer, intent(out) :: left_point, right_point
    real(dp), intent(out) :: fraction

    integer :: point

    left_point = 1
    right_point = 1
    fraction = 0._dp
    if (age <= days(1)) return

    do point = 2, size(days)
        if (age > days(point)) cycle
        right_point = point
        if (days(point) > days(point-1)) then
            left_point = point - 1
            fraction = real(age-days(point-1),dp)/real(days(point)-days(point-1),dp)
        else
            left_point = point
        end if
        return
    end do

    left_point = size(days)
    right_point = left_point
end subroutine curve_day_interval

pure real(dp) function curve_value(values, left_point, right_point, fraction) result(value)
    real(dp), intent(in) :: values(:)
    real(dp), intent(in) :: fraction
    integer, intent(in) :: left_point, right_point

    if (left_point == right_point) then
        value = values(left_point)
    else
        value = values(left_point) + (values(right_point)-values(left_point))*fraction
    end if
end function curve_value

real(dp) function curve_at_gdd(gdd_points, values, accumulated_gdd) result(value)
    ! Interpolate today's parameter directly from accumulated thermal time.
    real(dp), intent(in) :: gdd_points(:), values(:), accumulated_gdd
    integer :: point

    value = nan_r
    if (size(gdd_points) == 0) return
    if (accumulated_gdd <= gdd_points(1)) then
        value = values(1)
        return
    end if
    do point = 2, size(gdd_points)
        if (accumulated_gdd <= gdd_points(point)) then
            if (gdd_points(point) > gdd_points(point-1)) then
                value = values(point-1) + (values(point) - values(point-1)) * &
                      & (accumulated_gdd - gdd_points(point-1))/(gdd_points(point) - gdd_points(point-1))
            else
                value = values(point)
            end if
            return
        end if
    end do
    value = values(size(values))
end function curve_at_gdd

subroutine populate_crop_today(pheno, crop, kcb_correction, i, j)
    type(crop_pars_matrices), intent(inout) :: pheno
    type(crop_definition), intent(in) :: crop
    real(dp), intent(in) :: kcb_correction
    integer, intent(in) :: i, j
    real(dp) :: current_gdd, fraction
    integer :: first_point, last_point, age, left_point, right_point

    ! Multi-cut crops reset their GDD curve for each cut, even though pheno%gdd continues to accumulate
    current_gdd = pheno%gdd(i,j) - real(pheno%cuts_completed(i,j), dp) * maxval(crop%gdd)
    first_point = pheno%cuts_completed(i,j)*size(crop%gdd) + 1
    last_point = first_point + size(crop%gdd) - 1
    age = pheno%crop_age_days(i,j)

    ! Assign time-varying crop parameters
    if (any(pheno%curve_day(i,j,first_point:last_point) == huge(0))) then
        ! Fallback path: parameters vary linearly with accumulated GDD
        pheno%k_cb(i,j)     = curve_at_gdd(crop%gdd, crop%k_cb,              current_gdd)
        pheno%lai(i,j)      = curve_at_gdd(crop%gdd, crop%lai,               current_gdd)
        pheno%h(i,j)        = curve_at_gdd(crop%gdd, crop%height,            current_gdd)
        pheno%d_r(i,j)      = curve_at_gdd(crop%gdd, crop%root_depth,        current_gdd)
        pheno%f_c(i,j)      = curve_at_gdd(crop%gdd, crop%cover_fraction,    current_gdd)
        pheno%r_stress(i,j) = curve_at_gdd(crop%gdd, crop%stress_resistance, current_gdd)
        pheno%cn_day(i,j)=nint(curve_at_gdd(crop%gdd,crop%cn_value,          current_gdd))
    else
        ! Normal path: parameters vary linearly in time (standard FAO56 shape)
        call curve_day_interval(pheno%curve_day(i,j,first_point:last_point), age, left_point, right_point, fraction)
        pheno%k_cb(i,j)     = curve_value(crop%k_cb,              left_point, right_point, fraction)
        pheno%lai(i,j)      = curve_value(crop%lai,               left_point, right_point, fraction)
        pheno%h(i,j)        = curve_value(crop%height,            left_point, right_point, fraction)
        pheno%d_r(i,j)      = curve_value(crop%root_depth,        left_point, right_point, fraction)
        pheno%f_c(i,j)      = curve_value(crop%cover_fraction,    left_point, right_point, fraction)
        pheno%r_stress(i,j) = curve_value(crop%stress_resistance, left_point, right_point, fraction)
        pheno%cn_day(i,j)=nint(curve_value(crop%cn_value,         left_point, right_point, fraction))
    end if

    if (current_gdd >= crop%mid_start_gdd .and. pheno%k_cb(i,j) > 0.45_dp) then
        pheno%k_cb(i,j) = pheno%k_cb(i,j) + kcb_correction
    end if

    ! Cache static crop parameters in pheno matrices (easier to use for the rest of the code compared to accessing individual crop definitions)
    pheno%cn_class(i,j) = crop%cn_class
    pheno%irrigation_class(i,j) = merge(1, 0, crop%is_irrigated)
    pheno%p(i,j) = crop%raw_fraction
    pheno%a(i,j) = crop%interception_coef
    pheno%d_t_max(i,j) = maxval(crop%root_depth)
    pheno%RF_t_max(i,j) = crop%maximum_transpirative_root_fraction

    ! Update growth stage
    pheno%pheno_stage(i,j) = crop_growth_stage(crop, current_gdd)
end subroutine populate_crop_today

! Calculate GDD thresholds for crop growth stages once per crop and store them for later use
subroutine prepare_crop_stage_boundaries(crop)
    type(crop_definition), intent(inout) :: crop

    integer :: initial_start, initial_end, mid_start, mid_end, point
    real(dp) :: k_cb_mid

    crop%emergence_gdd = huge(0._dp)
    crop%initial_end_gdd = huge(0._dp)
    crop%mid_start_gdd = huge(0._dp)
    crop%mid_end_gdd = huge(0._dp)

    initial_start = 0
    do point = 1, size(crop%k_cb)
        if (crop%k_cb(point) > 0._dp) then
            initial_start = point
            exit
        end if
    end do
    if (initial_start == 0) return

    crop%emergence_gdd = crop%gdd(initial_start)
    initial_end = initial_start
    do point = initial_start + 1, size(crop%k_cb)
        if (crop%k_cb(point) /= crop%k_cb(initial_start)) exit
        initial_end = point
    end do
    if (initial_end == size(crop%k_cb)) return ! Constant positive Kcb stays in stage 1.
    crop%initial_end_gdd = crop%gdd(initial_end)

    k_cb_mid = maxval(crop%k_cb)
    mid_start = 0
    do point = initial_end + 1, size(crop%k_cb)
        if (crop%k_cb(point) == k_cb_mid) then
            mid_start = point
            exit
        end if
    end do
    if (mid_start == 0) return ! The peak lies within the initial segment.

    crop%mid_start_gdd = crop%gdd(mid_start)
    mid_end = mid_start
    do point = mid_start + 1, size(crop%k_cb)
        if (crop%k_cb(point) /= k_cb_mid) exit
        mid_end = point
    end do
    crop%mid_end_gdd = crop%gdd(mid_end)
end subroutine prepare_crop_stage_boundaries

! 0 = pre-emergence, 1 = initial, 2 = development, 3 = mid-season, 4 = late-season.
! Stages 1 and 3 usually correspond to Kcb plateaus - see FAO56
integer function crop_growth_stage(crop, accumulated_gdd) result(stage)
    type(crop_definition), intent(in) :: crop
    real(dp), intent(in) :: accumulated_gdd

    stage = 0
    if (accumulated_gdd < crop%emergence_gdd) return

    stage = 1
    if (accumulated_gdd <= crop%initial_end_gdd) return

    if (crop%mid_start_gdd == huge(0._dp)) then
        stage = 4
        return
    end if

    stage = 2
    if (accumulated_gdd < crop%mid_start_gdd) return

    stage = 3
    if (accumulated_gdd <= crop%mid_end_gdd) return

    stage = 4
end function crop_growth_stage

subroutine set_bare_soil_today(pheno, bare_soil, i, j)
    type(crop_pars_matrices), intent(inout) :: pheno
    type(crop_definition), intent(in) :: bare_soil
    integer, intent(in) :: i, j

    pheno%crop_id(i,j) = 0
    pheno%pheno_stage(i,j) = 0
    pheno%is_real_crop(i,j) = .false.
    pheno%k_cb(i,j) = bare_soil%k_cb(1)
    pheno%h(i,j) = bare_soil%height(1)
    pheno%d_r(i,j) = bare_soil%root_depth(1)
    pheno%lai(i,j) = bare_soil%lai(1)
    pheno%cn_day(i,j) = nint(bare_soil%cn_value(1))
    pheno%f_c(i,j) = bare_soil%cover_fraction(1)
    pheno%r_stress(i,j) = bare_soil%stress_resistance(1)
    pheno%irrigation_class(i,j) = merge(1, 0, bare_soil%is_irrigated)
    pheno%cn_class(i,j) = bare_soil%cn_class
    pheno%p(i,j) = bare_soil%raw_fraction
    pheno%a(i,j) = bare_soil%interception_coef
    pheno%d_t_max(i,j) = maxval(bare_soil%root_depth)
    pheno%RF_t_max(i,j) = bare_soil%maximum_transpirative_root_fraction
end subroutine set_bare_soil_today

end module mod_cropcoef
