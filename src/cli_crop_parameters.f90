module cli_crop_parameters
use mod_constants, only: dp, nan_r
use mod_date, only: date, annual_period_end, parse_day_month_date
use mod_utility, only: clean_input_line, join_path, lower_case, replace_str
use mod_parameters, only: simulation
use mod_crop_phenology
use mod_cropcoef, only: compute_canopy_resistance, prepare_crop_stage_boundaries
use mod_system
implicit none


contains

subroutine initialize_co2_parameters(sim)
    ! Use the same year/CO2 table accepted by CropCoef. Canopy resistance
    ! and adjusted crop productivity are calculated within IdrAgra.
    type(simulation), intent(inout) :: sim
    character(len=500) :: filename
    character(len=1000) :: line
    type(date) :: warmup_end
    integer :: unit, ios, year, idx, last_needed_year, n_years
    real(dp) :: co2

    filename = sim%co2_conc_fn
    last_needed_year = sim%end%year
    if (.not. sim%f_init_wc) then
        warmup_end = annual_period_end(sim%start)
        last_needed_year = max(last_needed_year, warmup_end%year)
    end if
    n_years = last_needed_year - sim%start_year + 1
    allocate(sim%co2_conc(n_years))
    allocate(sim%res_canopy(n_years))
    sim%co2_conc = -1._dp

    open(newunit=unit, file=trim(filename), status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'Cannot open CO2 concentration series ', trim(filename), '. Execution will be aborted...'
        stop
    end if

    do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        line = clean_input_line(line)
        if (len_trim(line) == 0) cycle
        read(line, *, iostat=ios) year, co2
        if (ios /= 0) cycle ! Header: Year CO2
        idx = year - sim%start_year + 1
        if (idx < 1 .or. idx > n_years) cycle
        sim%co2_conc(idx) = co2
    end do
    close(unit)

    if (any(sim%co2_conc <= 0._dp)) then
        print *, 'CO2 series ', trim(filename), ' must include every calendar year from ', &
                 & sim%start_year, ' to ', last_needed_year, '.'
        print *, 'Execution will be aborted...'
        stop
    end if

    do idx = 1, n_years
        sim%res_canopy(idx) = compute_canopy_resistance(sim%co2_conc(idx))
    end do
end subroutine initialize_co2_parameters

! Read all of the crop .dat files and the crop rotation file, and store the information in crops and crop_rotations.
subroutine import_crop_definitions(sim, crops, crop_rotations, bare_soil)
    type(simulation), intent(inout) :: sim
    type(crop_definition), dimension(:), allocatable, intent(out) :: crops
    type(crop_rotation), dimension(:), allocatable, intent(out) :: crop_rotations
    type(crop_definition), intent(out) :: bare_soil

    character(len=255), dimension(:), allocatable :: crop_files
    character(len=255), dimension(:,:), allocatable :: rotation_files
    integer, dimension(:), allocatable :: crop_ids
    character(len=1000) :: line
    character(len=255) :: first_crop, second_crop
    character(len=500) :: rotations_file
    integer :: unit, ios, land_use_id, expected_land_use_id
    integer :: rotation_idx, crop_idx, file_idx, n_rotations, n_crop_files
    integer :: explicit_id, maximum_crop_id

    rotations_file = join_path(sim%crop_inputs_path, sim%soil_uses_fn, delimiter)
    n_rotations = count_crop_rotations(rotations_file)
    sim%n_lus = n_rotations

    call initialize_bare_soil(bare_soil)

    allocate(crop_rotations(n_rotations))
    allocate(rotation_files(n_rotations, 2))
    allocate(crop_files(2*n_rotations), crop_ids(2*n_rotations))
    rotation_files = ''
    crop_files = ''
    crop_ids = 0
    n_crop_files = 0

    open(newunit=unit, file=trim(rotations_file), status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'Cannot open crop rotation file ', trim(rotations_file), '.'
        print *, 'Execution will be aborted...'
        stop
    end if

    rotation_idx = 0
    expected_land_use_id = 1
    do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        line = clean_input_line(line)
        if (len_trim(line) == 0) cycle

        first_crop = ''
        second_crop = ''
        read(line, *, iostat=ios) land_use_id, first_crop, second_crop
        if (ios /= 0) cycle

        if (land_use_id /= expected_land_use_id) then
            print *, 'Crop rotation IDs must be consecutive integers starting from 1.'
            print *, 'Expected/found: ', expected_land_use_id, land_use_id, ' in ', trim(rotations_file)
            print *, 'Execution will be aborted...'
            stop
        end if

        rotation_idx = rotation_idx + 1
        crop_rotations(rotation_idx)%land_use_id = land_use_id
        rotation_files(rotation_idx, 1) = trim(first_crop)
        if (trim(second_crop) /= '*') rotation_files(rotation_idx, 2) = trim(second_crop)

        do crop_idx = 1, 2
            if (len_trim(rotation_files(rotation_idx, crop_idx)) == 0) cycle
            file_idx = find_crop_file(crop_files, n_crop_files, rotation_files(rotation_idx, crop_idx))
            if (file_idx == 0) then
                n_crop_files = n_crop_files + 1
                crop_files(n_crop_files) = rotation_files(rotation_idx, crop_idx)
            end if
        end do
        expected_land_use_id = expected_land_use_id + 1
    end do
    close(unit)

    ! Numeric filename prefixes are stable crop IDs. Unprefixed files receive deterministic IDs after the largest explicit one
    maximum_crop_id = 0
    do file_idx = 1, n_crop_files
        explicit_id = crop_id_from_filename(crop_files(file_idx))
        if (explicit_id <= 0) cycle
        crop_ids(file_idx) = explicit_id
        maximum_crop_id = max(maximum_crop_id, explicit_id)
    end do

    do file_idx = 1, n_crop_files
        if (crop_ids(file_idx) > 0) cycle
        maximum_crop_id = maximum_crop_id + 1
        crop_ids(file_idx) = maximum_crop_id
    end do

    allocate(crops(maximum_crop_id))
    do file_idx = 1, n_crop_files
        crop_idx = crop_ids(file_idx)
        crops(crop_idx)%crop_id = crop_idx
        crops(crop_idx)%parameter_file = trim(crop_files(file_idx))
        call read_crop_file(join_path(sim%crop_parameters_path, crop_files(file_idx), delimiter), crops(crop_idx))
        if (is_bare_soil_file(crop_files(file_idx)) .and. len_trim(bare_soil%parameter_file) == 0) then
            bare_soil = crops(crop_idx)
            bare_soil%crop_id = 0
            print *, 'Using ', trim(crop_files(file_idx)), ' parameters as bare soil.'
        end if
    end do

    do rotation_idx = 1, n_rotations
        crop_idx = 1
        if (len_trim(rotation_files(rotation_idx, 2)) > 0) crop_idx = 2
        allocate(crop_rotations(rotation_idx)%crop_ids(crop_idx))
        do file_idx = 1, crop_idx
            crop_rotations(rotation_idx)%crop_ids(file_idx) = &
                & crop_ids(find_crop_file(crop_files, n_crop_files, rotation_files(rotation_idx, file_idx)))
        end do
    end do
end subroutine import_crop_definitions

! Default values for bare soil. These are overwritten if a 0_*.dat crop file is present
subroutine initialize_bare_soil(bare_soil)
    type(crop_definition), intent(out) :: bare_soil

    allocate(bare_soil%gdd(1), bare_soil%k_cb(1), bare_soil%height(1), bare_soil%root_depth(1),                  &
           & bare_soil%lai(1), bare_soil%cn_value(1), bare_soil%cover_fraction(1), bare_soil%stress_resistance(1))
    bare_soil%gdd = 0._dp
    bare_soil%k_cb = 0._dp
    bare_soil%height = 0._dp
    bare_soil%root_depth = 0._dp
    bare_soil%lai = 0._dp
    bare_soil%cn_value = 0._dp
    bare_soil%cover_fraction = 0._dp
    bare_soil%stress_resistance = 0._dp
    bare_soil%cn_class = 1
    bare_soil%maximum_transpirative_root_fraction = 0._dp
end subroutine initialize_bare_soil

integer function count_crop_rotations(file_name)
    character(len=*), intent(in) :: file_name
    character(len=1000) :: line
    character(len=255) :: first_crop, second_crop
    integer :: unit, ios, land_use_id

    count_crop_rotations = 0
    open(newunit=unit, file=trim(file_name), status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'Cannot open crop rotation file ', trim(file_name), '.'
        print *, 'Execution will be aborted...'
        stop
    end if

    do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        line = clean_input_line(line)
        read(line, *, iostat=ios) land_use_id, first_crop, second_crop
        if (ios == 0) count_crop_rotations = count_crop_rotations + 1
    end do
    close(unit)
end function count_crop_rotations

integer function find_crop_file(crop_files, n_crop_files, file_name)
    character(len=255), dimension(:), intent(in) :: crop_files
    integer, intent(in) :: n_crop_files
    character(len=*), intent(in) :: file_name
    character(len=255) :: stored_name, requested_name
    integer :: idx

    requested_name = trim(file_name)
    call lower_case(requested_name)
    find_crop_file = 0
    do idx = 1, n_crop_files
        stored_name = trim(crop_files(idx))
        call lower_case(stored_name)
        if (trim(stored_name) == trim(requested_name)) then
            find_crop_file = idx
            return
        end if
    end do
end function find_crop_file

integer function crop_id_from_filename(file_name)
    character(len=*), intent(in) :: file_name
    character(len=255) :: base_name
    integer :: separator, underscore, ios

    separator = max(scan(trim(file_name), '/', back=.true.), scan(trim(file_name), achar(92), back=.true.))
    base_name = file_name(separator + 1:)
    underscore = scan(trim(base_name), '_')
    crop_id_from_filename = 0
    if (underscore <= 1) return
    read(base_name(:underscore - 1), *, iostat=ios) crop_id_from_filename
    if (ios /= 0 .or. crop_id_from_filename <= 0) crop_id_from_filename = 0
end function crop_id_from_filename

!%PS%: note that the bare soil file can only be found if at least one landuse references it. Being in the landuses folder is not enough. Todo!
logical function is_bare_soil_file(file_name)
    character(len=*), intent(in) :: file_name
    integer :: separator

    separator = max(scan(trim(file_name), '/', back=.true.), scan(trim(file_name), achar(92), back=.true.))
    is_bare_soil_file = index(file_name(separator + 1:), '0_') == 1
end function is_bare_soil_file

subroutine read_crop_file(file_name, crop)
    character(len=*), intent(in) :: file_name
    type(crop_definition), intent(inout) :: crop
    character(len=2000) :: line, value, lower_line
    character(len=100) :: label
    real(dp), dimension(9) :: row
    integer :: unit, ios, separator, binary_flag
    logical :: reading_table

    crop%crop_name = crop_name_from_filename(file_name)
    crop%parameter_file = trim(file_name)
    allocate(crop%gdd(0), crop%k_cb(0), crop%lai(0), crop%height(0), crop%root_depth(0), &
           & crop%yield_response(0), crop%cn_value(0), crop%cover_fraction(0), crop%stress_resistance(0))

    open(newunit=unit, file=trim(file_name), status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'Cannot open crop parameter file ', trim(file_name), '.'
        print *, 'Execution will be aborted...'
        stop
    end if

    reading_table = .false.
    do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        line = clean_input_line(line)
        if (len_trim(line) == 0) cycle

        lower_line = trim(line)
        call lower_case(lower_line)
        if (trim(lower_line) == 'endtable') then
            reading_table = .false.
            cycle
        end if
        if (index(adjustl(lower_line), 'gdd') == 1) then
            reading_table = .true.
            cycle
        end if

        if (reading_table) then
            row = nan_r
            value = replace_str(trim(line), '*', '-9999')
            read(value, *, iostat=ios) row
            if (row(1) == nan_r) then
                print *, 'Invalid GDD curve row in ', trim(file_name), ': ', trim(line)
                print *, 'Execution will be aborted...'
                stop
            end if
            call append_crop_curve_row(crop, row)
            cycle
        end if

        separator = scan(line, '=')
        if (separator == 0) cycle
        label = adjustl(trim(line(:separator - 1)))
        value = adjustl(trim(line(separator + 1:)))
        call lower_case(label)
        select case (trim(label))
            case ('sowingdate_min'); read(value, *, iostat=ios) crop%sowing_doy_min
            case ('sowingdelay_max'); read(value, *, iostat=ios) crop%sowing_delay_max
            case ('harvestdate_max'); read(value, *, iostat=ios) crop%harvest_doy_max
            case ('harvnum_max'); read(value, *, iostat=ios) crop%max_cuts
            case ('cropsoverlap'); read(value, *, iostat=ios) crop%crop_overlap_days
            case ('tsowing'); read(value, *, iostat=ios) crop%sowing_temp
            case ('tdaybase'); read(value, *, iostat=ios) crop%base_temp
            case ('tcutoff'); read(value, *, iostat=ios) crop%cutoff_temp
            case ('vern'); read(value, *, iostat=ios) binary_flag; if (ios == 0) crop%requires_vernalization = binary_flag /= 0
            case ('tv_min'); read(value, *, iostat=ios) crop%vern_temp_min
            case ('tv_max'); read(value, *, iostat=ios) crop%vern_temp_max
            case ('vfmin'); read(value, *, iostat=ios) crop%vern_fact_min
            case ('vstart'); read(value, *, iostat=ios) crop%vern_days_start
            case ('vend'); read(value, *, iostat=ios) crop%vern_days_end
            case ('vslope'); read(value, *, iostat=ios) crop%vern_curve_slope
            case ('ph_r'); read(value, *, iostat=ios) crop%photoperiod_response
            case ('daylength_if'); read(value, *, iostat=ios) crop%daylength_if
            case ('daylength_ins'); read(value, *, iostat=ios) crop%daylength_ins
            case ('wp'); read(value, *, iostat=ios) crop%water_productivity
            case ('fsink'); read(value, *, iostat=ios) crop%sink_strength
            case ('tcrit_hs'); read(value, *, iostat=ios) crop%heat_stress_temp_crit
            case ('tlim_hs'); read(value, *, iostat=ios) crop%heat_stress_temp_lim
            case ('hi'); read(value, *, iostat=ios) crop%harvest_index
            case ('kyt'); read(value, *, iostat=ios) crop%yield_response_total
            case ('ky1'); read(value, *, iostat=ios) crop%yield_response_stage(1)
            case ('ky2'); read(value, *, iostat=ios) crop%yield_response_stage(2)
            case ('ky3'); read(value, *, iostat=ios) crop%yield_response_stage(3)
            case ('ky4'); read(value, *, iostat=ios) crop%yield_response_stage(4)
            case ('praw'); read(value, *, iostat=ios) crop%raw_fraction
            case ('ainterception'); read(value, *, iostat=ios) crop%interception_coef
            case ('cl_cn'); read(value, *, iostat=ios) crop%cn_class
            case ('irrigation'); read(value, *, iostat=ios) binary_flag; if (ios == 0) crop%is_irrigated = binary_flag /= 0
            case ('adj_flag'); read(value, *, iostat=ios) binary_flag; if (ios == 0) crop%adjust_k_cb = binary_flag /= 0
            case ('kcbcorrectionstart'); call parse_day_month_date(value, crop%kcb_corr_start_day, crop%kcb_corr_start_month,ios)
            case ('kcbcorrectionend'); call parse_day_month_date(value, crop%kcb_corr_end_day, crop%kcb_corr_end_month, ios)
            case ('rft'); read(value, *, iostat=ios) crop%maximum_transpirative_root_fraction
            case default
                print *, 'Skipping invalid or obsolete label <',trim(label),'> in ', trim(file_name)
                cycle
        end select

        if (ios /= 0) then
            print *, 'Invalid value for ', trim(label), ' in ', trim(file_name), '.'
            print *, 'Execution will be aborted...'
            stop
        end if
    end do
    close(unit)

    if (size(crop%gdd) == 0) then
        print *, 'Crop parameter file has no GDD curve rows: ', trim(file_name), '.'
        print *, 'Execution will be aborted...'
        stop
    end if

    if (crop%max_cuts < 1) then
        print *, 'HarvNum_max must be at least 1 in ', trim(file_name), '.'
        print *, 'Execution will be aborted...'
        stop
    end if

    call prepare_crop_definition_curves(crop)

end subroutine read_crop_file

subroutine append_crop_curve_row(crop, row)
    type(crop_definition), intent(inout) :: crop
    real(dp), dimension(9), intent(in) :: row

    crop%gdd = [crop%gdd, row(1)]
    crop%k_cb = [crop%k_cb, row(2)]
    crop%lai = [crop%lai, row(3)]
    crop%height = [crop%height, row(4)]
    crop%root_depth = [crop%root_depth, row(5)]
    crop%cn_value = [crop%cn_value, row(6)]
    crop%cover_fraction = [crop%cover_fraction, row(7)]
    crop%stress_resistance = [crop%stress_resistance, row(8)]
    crop%yield_response = [crop%yield_response, row(9)]
end subroutine append_crop_curve_row

subroutine prepare_crop_definition_curves(crop)
    ! Match CropCoef's preprocessing of missing curve values so the numerical
    ! module can consume complete parameter curves directly.
    type(crop_definition), intent(inout) :: crop
    integer :: idx

    call fill_missing_curve(crop%k_cb, crop%gdd)
    call prepare_crop_stage_boundaries(crop)

    call fill_missing_curve(crop%lai, crop%gdd)
    call fill_missing_curve(crop%height, crop%gdd)
    call fill_missing_curve(crop%root_depth, crop%gdd)
    call fill_missing_curve(crop%yield_response, crop%gdd)
    call fill_missing_curve(crop%cover_fraction, crop%gdd)

    if (all(crop%stress_resistance == nan_r)) then
        crop%stress_resistance = 0.0_dp
    else
        call fill_missing_curve(crop%stress_resistance)
    end if

    do idx = 1, size(crop%cn_value)
        if (crop%cn_value(idx) /= nan_r) cycle
        crop%cn_value(idx) = 1.0_dp
        if (crop%k_cb(idx) == 0.0_dp) crop%cn_value(idx) = 0.0_dp
        if (crop%k_cb(idx) >= 0.45_dp) crop%cn_value(idx) = 2.0_dp
    end do

    crop%yield_response(1) = crop%yield_response_stage(1)
    crop%yield_response(size(crop%yield_response)) = crop%yield_response_stage(4)
    do idx = 1, size(crop%yield_response)
        if (crop%yield_response(idx) /= nan_r) cycle
        if (crop%k_cb(idx) < 0.45_dp) then
            crop%yield_response(idx) = crop%yield_response_stage(2)
        else
            crop%yield_response(idx) = crop%yield_response_stage(3)
        end if
    end do
end subroutine prepare_crop_definition_curves

! Analogous to CropCoef's fillMissingL()
subroutine fill_missing_curve(values, abscissa)
    real(dp), dimension(:), intent(inout) :: values
    real(dp), dimension(:), optional, intent(in) :: abscissa
    real(dp), dimension(size(values)) :: x
    integer, dimension(size(values)) :: known
    integer :: idx, pair_idx, start_idx, end_idx, n_known
    real(dp) :: slope

    if (present(abscissa)) then
        x = abscissa
    else
        x = [(real(idx, dp), idx=1,size(values))]
    end if

    known = 0
    n_known = 0
    do idx = 1, size(values)
        if (values(idx) == nan_r) cycle
        n_known = n_known + 1
        known(n_known) = idx
    end do
    do pair_idx = 1, n_known - 1
        start_idx = known(pair_idx)
        end_idx = known(pair_idx + 1)
        if (end_idx <= start_idx + 1) cycle
        if (x(end_idx) == x(start_idx)) then
            slope = 0._dp
        else
            slope = (values(end_idx) - values(start_idx))/(x(end_idx) - x(start_idx))
        end if
        do idx = start_idx + 1, end_idx - 1
            values(idx) = values(start_idx) + slope*(x(idx) - x(start_idx))
        end do
    end do
end subroutine fill_missing_curve

function crop_name_from_filename(file_name) result(crop_name)
    character(len=*), intent(in) :: file_name
    character(len=255) :: crop_name
    integer :: separator, extension

    separator = max(scan(trim(file_name), '/', back=.true.), scan(trim(file_name), achar(92), back=.true.))
    crop_name = file_name(separator + 1:)
    extension = scan(trim(crop_name), '.', back=.true.)
    if (extension > 1) crop_name = crop_name(:extension - 1)
end function crop_name_from_filename

end module cli_crop_parameters
