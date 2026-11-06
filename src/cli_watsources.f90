module cli_watsources
use mod_constants, only: dp, seconds_per_day
use mod_date, only: date, split_date, days_between_dates, dates_are_equal
use mod_utility, only: get_value_index, lower_case
use mod_parameters
use mod_grid, only: grid_i, grid_r
use mod_meteo, only: meteo_info
implicit none

contains

subroutine open_daily_discharges_file(file_name, mn_src_tbl, source_nr)
    ! open the file in the following format
    ! 3     4   ...
    ! 10.0  6.0 ...
    ! 01/01/2000 -> 31/12/2000
    ! 0.050 0.012   ...
    ! 0.070 0.045   ...
    ! 0.065 0.03    ...
    ! ...   ...     ...
    character(len=*), intent(in) :: file_name
    type(monitored_sources_table),intent(out)::mn_src_tbl
    integer, intent(out) :: source_nr
    integer :: i
    integer :: ios = 0
    integer :: n_rows
    character(len=300):: date_string, date_start, date_end
    character(len=999):: string                          ! File string
    integer:: strlen
    character(len=1),parameter :: delimiter = achar(9)   ! Delimiter: horizontal tab

    open(newunit=mn_src_tbl%unit, file=trim(file_name), status='old', action="read", iostat=ios)
    if (ios /= 0 ) then
        print *, "Cannot open file ", trim(file_name), ". The specified file does not exist. Execution will be aborted..."
        stop
    end if

    ! Read file header (considering also delimiters and spaces: it needs formatting)
    read(mn_src_tbl%unit,'(a)') string
    strlen = len(trim(string))

    ! Count the number of delimiters in the first line
    source_nr = 1
    do i = 1, strlen
        if (string(i:i) == delimiter) then
            if (i > 1 .and. string((i-1):(i-1)) /= delimiter) then ! To not take into account multiple delimiters
                source_nr = source_nr + 1
            end if
        end if
    end do

    ! count data
    rewind(mn_src_tbl%unit, iostat = ios)
    n_rows = -3 ! skip the first 3 lines that contain: ID, Qnom and dates
    do while (.true.)
        read(mn_src_tbl%unit, *, end=999) string
        n_rows = n_rows + 1
    end do
    999 continue

    ! allocate variable with source number
    allocate(mn_src_tbl%q_nom(source_nr))
    allocate(mn_src_tbl%wat_src_id(source_nr))

    ! load the data
    rewind(mn_src_tbl%unit, iostat = ios)
    read(mn_src_tbl%unit, *, iostat=ios) mn_src_tbl%wat_src_id  ! read the id of the source
    do i = 1, source_nr
        call lower_case(mn_src_tbl%wat_src_id(i))
    end do
    read(mn_src_tbl%unit, *, iostat=ios) mn_src_tbl%q_nom ! read the potential discharge

    ! Read the first and the last days, separated by " -> " : eg. "01/01/1993 -> 31/12/2014"
    read(mn_src_tbl%unit, '(a300)', iostat = ios) date_string
    call split_date(date_string, date_start, date_end)
    call split_date(date_start, mn_src_tbl%start)
    call split_date(date_end, mn_src_tbl%finish)
    ! Check if the length of the time series matches the dates limits
    if (n_rows /= days_between_dates(mn_src_tbl%start, mn_src_tbl%finish) + 1) then
        stop "Diversion time series have incoherent lengths with respect to declared dates. Execution will be aborted..."
    end if

end subroutine open_daily_discharges_file

subroutine read_unm_coll_sources_list(file_name, unm_coll_src_tbl, n_unm_coll_src, pars)
    ! open the file with the list of unmonitored collective sources (i.e. public wells)
    ! the file must be structured as follow (# are comments):
    ! # SourceWellTotNum: number of public wells
    ! CRS_TotNum = 1
    ! # List: Wells parameters' files
    ! # List starts with the label "List =" and ends with the label "EndList ="
    ! List =
    ! 60.txt
    ! EndList =
   character(len=*), intent(in):: file_name
    type(unmonitored_sources_table),intent(inout)::unm_coll_src_tbl
    type(parameters),intent(inout)::pars
    integer, intent(out):: n_unm_coll_src
    character(len=300) :: comment, buffer, label
    integer :: ios, p, line, tablestart

    integer::free_unit
    integer::i
    character(len=200)::temp

    n_unm_coll_src = 0
    ios = 0
    line = 0
    open(newunit=free_unit, file=file_name, action="read", iostat=ios)
    if (ios /= 0) then
        print *, "Error opening file ", file_name, "; iostat = ", ios, &
            & ". Execution will be aborted..."
        stop
    end if
    do while (ios == 0)
        read(free_unit, '(A)', iostat = ios) buffer
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
                    case ('crs_totnum', 'sourcewelltotnum')
                        read (buffer, *, iostat = ios) n_unm_coll_src
                        ! TODO: %AB% check missing value condition
                        allocate(pars%uc_act_rules(pars%cr%n_withdrawals))
                        allocate(unm_coll_src_tbl%q_max(n_unm_coll_src))
                        allocate(unm_coll_src_tbl%q_nom(n_unm_coll_src))
                        allocate(unm_coll_src_tbl%wat_src_id(n_unm_coll_src))
                        allocate(unm_coll_src_tbl%unit(n_unm_coll_src))
                    case ('list')
                        tablestart = line
                        do i=1, n_unm_coll_src
                            read (free_unit, *) temp
                            call read_unm_coll_source_par(trim(pars%sim%watsour_path)//trim(temp), unm_coll_src_tbl, i, pars)
                            line = line + 1
                        end do
                    case ('endlist')
                        if ((line - tablestart - 1) > n_unm_coll_src) then
                            print *, 'Collective runtime sources number (CRS_num) is higher than public wells listed. &
                                & Execution will be aborted...'
                            stop
                        else if ((line - tablestart - 1) < n_unm_coll_src) then
                            print *, 'Collective runtime sources number (CRS_num) is lower than public wells listed. &
                                & Execution will be aborted...'
                            stop
                        end if
                    case default
                        print *, 'Skipping invalid or obsolete label <',trim(label),'> at line', line, &
                            & ' of file: ', file_name
                end select
            end if
        end if
    end do
    close(free_unit)
end subroutine read_unm_coll_sources_list

subroutine read_unm_coll_source_par(file_name, unm_col_sour_tbl, k, pars)
   character(len=*), intent(in) :: file_name
    integer,intent(in)::k
    type(parameters),intent(inout)::pars
    type(unmonitored_sources_table),intent(out)::unm_col_sour_tbl

    integer :: i
    integer :: ios = 0
    character(len=300) :: comment, buffer, label, skip
    integer :: line = 0
    integer :: table_start = 0
    integer :: p

    ! open the file
    open(newunit=unm_col_sour_tbl%unit(k), file=trim(file_name), status='old', action="read", iostat=ios)
    if( ios /= 0 ) then
        print *, "Error opening file '", trim(file_name), "'; iostat=", ios
        print *, 'Execution will be aborted...'
        stop
    end if

    do while (ios == 0)
        read (unm_col_sour_tbl%unit(k), '(A)', iostat = ios) buffer
        if (ios == 0) then
            line = line + 1
            ! remove white spaces before and after string sequences
            buffer = trim(buffer)
            ! check if there are comment and get only left sie of the row
            p = scan(buffer, '#')
            if (p /= 0) then
                comment = buffer(p+1:)
                buffer = buffer (1:p-1)
            end if
            ! if buffer is different from blank string, parse it
            if (buffer /= '') then
                call lower_case(buffer)
                p = scan(buffer, '=')
                label = buffer(1:p-1)
                buffer = buffer(p+1:)
                select case (label)
                    case ('wellacronym', 'sourceacronym')
                        ! TODO: add error message in case of longer string
                        read(buffer, *, iostat = ios) unm_col_sour_tbl%wat_src_id(k)
                    case ('qmax') ! maximum flow rate
                        read(buffer, *, iostat = ios) unm_col_sour_tbl%q_max(k)
                    case ('qnom') ! nominal flow rate
                        read(buffer, *, iostat = ios) unm_col_sour_tbl%q_nom(k)
                    case ('actthrs ') ! minimum activation threshold
                        read(buffer, *, iostat = ios) pars%uc_act_rules(k)%min_act_trsld
                    case ('table') ! table with activation threshold and flow rate
                        table_start = line
                        read(unm_col_sour_tbl%unit(k), '(A)', iostat = ios) skip ! skip
                        do i = 1, 3
                            ! id | activation threshold | flow rate
                            read(unm_col_sour_tbl%unit(k), *) skip, pars%uc_act_rules(k)%act_trsld(i), pars%uc_act_rules(k)%flow_rate(i)
                            line = line + 1
                        end do
                    case ('endtable')
                        if ((line - table_start - 1) /= 3) then
                            print *, line - table_start - 1, "- instead of 3 - activation thresholds are listed"
                            print *, 'Execution will be aborted...'
                            stop
                        end if
                    case default ! all  other cases ...
                        print *, 'Skipping invalid or obsolete label <',trim(label),'> at line', line, &
                            & ' of file: ', trim(file_name)
                end select
            end if
        end if
    end do

    !!! if you want to make silent ACTTHRS:
    ! remove the reading part
    ! activate the following:
    ! pars%uc_act_rules(k)%actthrs = pars%uc_act_rules(k)%perc(1)

    ! check input
    do i = 1, 2
        if (pars%uc_act_rules(k)%act_trsld(i+1) >= pars%uc_act_rules(k)%act_trsld(i)) then
            print *, "Activation threshold ", i+1 , "(", pars%uc_act_rules(k)%act_trsld(i+1), ") of well ", trim(file_name), &
                & " is higher than activation threshold ", i, "(", pars%uc_act_rules(k)%act_trsld(i), ")"
            print *, 'Execution will be aborted...'
            stop
        else if (pars%uc_act_rules(k)%flow_rate(i+1) <= pars%uc_act_rules(k)%flow_rate(i)) then
            print *, "Flow rate ratio ", i+1, "(", pars%uc_act_rules(k)%flow_rate(i+1) , ") of well ", trim(file_name), &
                & " is lower than flow rate ratio ", i, "(", pars%uc_act_rules(k)%flow_rate(i), ")"
            print *, 'Execution will be aborted...'
            stop
        end if
    end do
    close(unm_col_sour_tbl%unit(k))

end subroutine read_unm_coll_source_par

subroutine open_scheduled_irrigation(file_name,sch_irr,debug)
    ! open scheduled irrigation file
    ! the shape of the file must be:
    !   - an integer value "n" defining the number of record
    !   - a field header line
    !   - a 1-n list of record with data in the form: | irrigation unit id | year | doy | water depth |
    !     separated by tab character or spaces
   character(len=*), intent(in) :: file_name
    logical, intent(in):: debug
    type(scheduled_irrigation),dimension(:),allocatable,intent(out)::sch_irr
    integer :: i,n_rec,free_unit
    integer :: ios

    ios=0

    ! open the file
    open(newunit=free_unit, file=trim(file_name), status='old', action="read", iostat=ios)
    if (ios /= 0 ) then
        print *, "Cannot open file ", trim(file_name), ". The specified file does not exist. Execution will be aborted..."
        stop
    end if

    read( free_unit, *, iostat=ios) n_rec           ! read the number of data records
    read( free_unit, *, iostat=ios)                 ! skip header
    allocate(sch_irr(n_rec))                        ! allocate array of type "scheduled_irrigation"
    ! loop throw records and populate sch_irr
    do i=1,size(sch_irr)
        read(free_unit,*) sch_irr(i)%irr_unit_id, sch_irr(i)%year, sch_irr(i)%doy, sch_irr(i)%h_irr
    end do
    ! close file
    close(free_unit)
    ! print debug
    if (debug) then
        print *,'===== DEBUG: scheduled irrigation ====='
        print *,'scheduled irrigation file: ',  file_name
        print *,'districtID year DoY waterDepth'
        do i=1,size(sch_irr)
            print *, sch_irr(i)%irr_unit_id,' ' , sch_irr(i)%year,' ', sch_irr(i)%doy, ' ', sch_irr(i)%h_irr
        end do
        print *,'===== END DEBUG ====='
    end if

end subroutine open_scheduled_irrigation

subroutine read_water_sources_table(wat_sour_tbl_fn, wat_sour_tbl)
    ! Read the list of water sources for each irrigation units.
    ! Each sources have an id, a type and the release ratio
    ! The shape of the file must be:
    ! DISTR_ID	SOURCE_CODE	SOURCE_TYPE	FLOW_RATIO
    !    1	           10   	1	        0.40
    !    1             60   	4       	0.13
    !   ...
    character(len=*), intent(in):: wat_sour_tbl_fn
    type(water_sources_table), dimension(:), allocatable,intent(inout) :: wat_sour_tbl

    integer :: i, free_unit
    integer :: ios = 0
    integer :: n_cols, n_rows, str_len
    character(len=999) :: string
    character(len=1), parameter :: delimiter = achar(9)

    ! open the file
    open(newunit=free_unit, file=trim(wat_sour_tbl_fn), status='old', action="read", iostat=ios)
    if(ios /= 0) then
        print *, "Error opening file '", trim(wat_sour_tbl_fn), "'; iostat=", ios, &
            & '. Execution will be aborted...'
        stop
    end if

    read(free_unit,'(a)') string    ! read file header (considering also delimiters and spaces: it needs formatting)
    str_len = len(trim(string))

    n_cols = 1                        ! Count the number of delimiters in the first line
    do i = 1, str_len
        if (string(i:i) == delimiter) then
            n_cols = n_cols+1
        end if
    end do
    if (n_cols /= 4) then
        print *, 'Error in file formatting: ', trim(wat_sour_tbl_fn), ' has ', n_cols, &
            & ' columns instead of 4. Execution will be aborted...'
        stop
    end if

    ! Count the number of rows
    n_rows = 0
    do while (.true.)
        read(free_unit, *, end=999) string
        n_rows = n_rows + 1
    end do
    999 continue

    if(.not.(allocated(wat_sour_tbl)))allocate(wat_sour_tbl(n_rows))

    ! load the data
    rewind(free_unit)                ! Rewind of file to store its content
    read(free_unit,*) string         ! Skip header line
    do i=1, n_rows
        read(free_unit, *, iostat=ios) wat_sour_tbl(i)%id_irr_unit,wat_sour_tbl(i)%id_wat_src,wat_sour_tbl(i)%type_id,wat_sour_tbl(i)%duty_frc
        call lower_case(wat_sour_tbl(i)%id_wat_src)
    end do
    close(free_unit)

end subroutine read_water_sources_table

subroutine init_water_sources_duty(pars,wat_src_tbl,src_info,weather_info)
    ! init water sources and water sources table
    type(parameters),intent(inout)::pars
    type(water_sources_table),dimension(:),allocatable,intent(inout)::wat_src_tbl
    type(source_info),intent(inout)::src_info
    type(meteo_info),dimension(:),intent(in)::weather_info

    integer::i

    call read_water_sources_table(trim(pars%sim%watsour_path)//trim(pars%sim%watsources_fn), wat_src_tbl)

    nullify(src_info%mn_src_tbl1%q_daily)
    nullify(src_info%mn_src_tbl2%q_daily)
    nullify(src_info%int_reuse_tbl%q_daily)

    if(pars%ms_i%f_exists .eqv. .true.)then
        call open_daily_discharges_file(trim(pars%sim%watsour_path)//trim(pars%sim%mon_sources_i_div_fn),src_info%mn_src_tbl1, &
            & pars%ms_i%n_withdrawals)
        ! Check dates match weather data
        if (.not. dates_are_equal(src_info%mn_src_tbl1%start, weather_info(1)%start)) then
            stop 'Meteorological and monitored sources (i) time series start in different days'
        else if (.not. dates_are_equal(src_info%mn_src_tbl1%finish, weather_info(1)%finish)) then
            stop 'Meteorological and monitored sources (i) time series end in different days'
        end if
    end if

    if(pars%ms_ii%f_exists .eqv. .true.)then
        call open_daily_discharges_file(trim(pars%sim%watsour_path)//trim(pars%sim%mon_sources_ii_div_fn),src_info%mn_src_tbl2, &
            & pars%ms_ii%n_withdrawals)
        if (pars%ms_i%f_exists .eqv. .true.) then ! check dates match between different monitored water sources
            if (.not. dates_are_equal(src_info%mn_src_tbl1%start, src_info%mn_src_tbl2%start)) then
                stop 'monitored sources (i) and monitored sources (ii) time series start in different days'
            else if (.not. dates_are_equal(src_info%mn_src_tbl1%finish, src_info%mn_src_tbl2%finish)) then
                stop 'monitored sources (i) and monitored sources (ii) series end in different days'
            end if
        else  ! Check dates match weather data
            if (.not. dates_are_equal(src_info%mn_src_tbl2%start, weather_info(1)%start)) then
                stop 'Meteorological and monitored sources (ii) time series start in different days'
            else if (.not. dates_are_equal(src_info%mn_src_tbl2%finish, weather_info(1)%finish)) then
                stop 'Meteorological and monitored sources (ii) time series end in different days'
            end if
        end if
    end if

    if(pars%intreu%f_exists .eqv. .true.)then
        call open_daily_discharges_file(trim(pars%sim%watsour_path)//trim(pars%sim%int_reuse_div_fn), &
            src_info%int_reuse_tbl, pars%intreu%n_withdrawals)
        if (pars%ms_ii%f_exists .eqv. .true.) then ! Check dates match monitored water sources 2
            if (.not. dates_are_equal(src_info%mn_src_tbl2%start, src_info%int_reuse_tbl%start)) then
                stop 'monitored sources (ii) and internal reuse time series start in different days'
            else if (.not. dates_are_equal(src_info%mn_src_tbl2%finish, src_info%int_reuse_tbl%finish)) then
                stop 'monitored sources (ii) and internal reuse time series end in different days'
            end if
        else if (pars%ms_i%f_exists .eqv. .true.) then ! Check dates match monitored water sources 1
            if (.not. dates_are_equal(src_info%mn_src_tbl1%start, src_info%int_reuse_tbl%start)) then
                stop 'monitored sources (i) and internal reuse time series start in different days'
            else if (.not. dates_are_equal(src_info%mn_src_tbl1%finish, src_info%int_reuse_tbl%finish)) then
                stop 'monitored sources (i) and internal reuse time series end in different days'
            end if
        else ! Check dates match weather data
            if (.not. dates_are_equal(src_info%int_reuse_tbl%start, weather_info(1)%start)) then
                stop 'Meteorological and internal reuse time series start in different days'
            else if (.not. dates_are_equal(src_info%int_reuse_tbl%finish, weather_info(1)%finish)) then
                stop 'Meteorological and internal reuse time series end in different days'
            end if
        end if
    end if

    if(pars%cr%f_exists .eqv. .true.)then
        call read_unm_coll_sources_list(trim(pars%sim%watsour_path)//trim(pars%sim%cr_sources_list_fn), src_info%unm_src_tbl, &
            & pars%cr%n_withdrawals, pars)
    end if

    ! Assign column index to each water sources
    do i=1,size(wat_src_tbl)
        !print*,i,wat_src_tbl(i)%id_irr_unit,wat_src_tbl(i)%id_wat_src,wat_src_tbl(i)%type_id
        select case(wat_src_tbl(i)%type_id)
            case(1)
                wat_src_tbl(i)%wat_src_idx = get_value_index(src_info%mn_src_tbl1%wat_src_id,wat_src_tbl(i)%id_wat_src)
            case(2)
                wat_src_tbl(i)%wat_src_idx = get_value_index(src_info%mn_src_tbl2%wat_src_id,wat_src_tbl(i)%id_wat_src)
            case(3)
                wat_src_tbl(i)%wat_src_idx = get_value_index(src_info%int_reuse_tbl%wat_src_id,wat_src_tbl(i)%id_wat_src)
            case(4)
                wat_src_tbl(i)%wat_src_idx = get_value_index(src_info%unm_src_tbl%wat_src_id,wat_src_tbl(i)%id_wat_src)
            case default
                stop "Code not identified in watsour%type column. Execution will be aborted..."
        end select
    end do

end subroutine init_water_sources_duty

subroutine close_water_sources_dudy(src_info,pars)
    type(source_info),intent(in)::src_info
    type(parameters),intent(in)::pars

    integer::ios =0

    if(pars%ms_i%f_exists .eqv. .true.)then
        close(src_info%mn_src_tbl1%unit,iostat=ios)
        if(ios/=0)then
            print *, "Error closing file ", trim(pars%sim%mon_sources_i_div_fn), " connected to unit =", src_info%mn_src_tbl1%unit, &
                & " iostat=", ios, ". Execution will be aborted..."
            stop
        end if
    end if

    if(pars%ms_ii%f_exists .eqv. .true.)then
        close(src_info%mn_src_tbl2%unit,iostat=ios)
        if(ios/=0)then
            print *, "Error closing file ", trim(pars%sim%mon_sources_ii_div_fn), " connected to unit =", src_info%mn_src_tbl2%unit, &
                & " iostat=", ios, ". Execution will be aborted..."
            stop
        end if
    end if

    if(pars%intreu%f_exists .eqv. .true.)then
        close(src_info%int_reuse_tbl%unit,iostat=ios)
        if(ios/=0)then
            print *, "Error closing file ", trim(pars%sim%int_reuse_div_fn), " connected to unit =", src_info%int_reuse_tbl%unit, &
                & " iostat=", ios, ". Execution will be aborted..."
            stop
        end if
    end if

end subroutine close_water_sources_dudy

subroutine nom_water_supply(watsources_fn, irr_units, src_info, wat_src_tbl, f_shapearea, cell_size, shape_area, &
                            & irr_unit_map, verbose)
   character(len=*), intent(in):: watsources_fn
    type(irr_units_table),dimension(:),intent(inout)::irr_units
    type(source_info),intent(in)::src_info
    type(water_sources_table),dimension(:),intent(in)::wat_src_tbl
    logical,intent(in)::f_shapearea
    real(dp),intent(in)::cell_size
    real(dp),dimension(:,:),intent(in)::shape_area
    integer,dimension(:,:),intent(in)::irr_unit_map
    logical,intent(in)::verbose
    integer::i,j,k
    real(dp) :: avg_cell_area

    irr_units%q_nom = 0
    irr_units(:)%q_pot_fld(1) = 0
    irr_units(:)%q_pot_fld(2) = 0
    irr_units(:)%q_pot_fld(3) = 0
    irr_units(:)%q_pot_fld(4) = 0

    do i=1,size(wat_src_tbl)
        j = wat_src_tbl(i)%irr_unit_idx; if(j==0) cycle
        k = wat_src_tbl(i)%wat_src_idx
        select case(wat_src_tbl(i)%type_id)
            case(1)                 ! 1st water sources
                irr_units(j)%q_pot_fld(1) = wat_src_tbl(i)%duty_frc*src_info%mn_src_tbl1%q_nom(k)*irr_units(j)%int_distr_eff + irr_units(j)%q_pot_fld(1)
            case(2)                 ! 2nd water sources
                irr_units(j)%q_pot_fld(2) = wat_src_tbl(i)%duty_frc*src_info%mn_src_tbl2%q_nom(k)*irr_units(j)%int_distr_eff + irr_units(j)%q_pot_fld(2)
            case(3)                 ! water reuse
                irr_units(j)%q_pot_fld(3) = wat_src_tbl(i)%duty_frc*src_info%int_reuse_tbl%q_nom(k)*irr_units(j)%int_distr_eff + irr_units(j)%q_pot_fld(3)
            case(4)                 ! unmonitored collective water sources
                irr_units(j)%q_pot_fld(4) = wat_src_tbl(i)%duty_frc*src_info%unm_src_tbl%q_nom(k)*irr_units(j)%int_distr_eff + irr_units(j)%q_pot_fld(4)
            case default
                print *,"File", trim(watsources_fn), " lists a source type that is not codified: ", wat_src_tbl(i)%type_id
        end select
    end do

    ! Total potential water discharge [m3/s]
    irr_units%q_nom = irr_units%q_pot_fld(1) + irr_units%q_pot_fld(2) + irr_units%q_pot_fld(3) + irr_units%q_pot_fld(4)

    ! Average number of irrigable cells per day
    irr_units(:)%n_irrigable_cells = 0._dp
    do i=1, size(irr_units)
        if (irr_units(i)%n_cells <= 0) then
            print *, "Irrigation unit ", irr_units(i)%id, " is outside the active domain."
        else if (irr_units(i)%h_irr_mean > 0._dp) then
            if (f_shapearea) then
                avg_cell_area = sum(shape_area, irr_unit_map == irr_units(i)%id) / irr_units(i)%n_cells
            else
                avg_cell_area = cell_size**2
            end if
            irr_units(i)%n_irrigable_cells = irr_units(i)%q_nom * seconds_per_day / &
                                             (1.e-3 * irr_units(i)%h_irr_mean * avg_cell_area)
        else
            print *, "Irrigation unit ", irr_units(i)%id, " contains no irrigable cells. h_irr_mean: ", irr_units(i)%h_irr_mean
        end if
    end do

    ! TODO: save as CSV file
    if (verbose .eqv. .true.) then
        print *,'===== DEBUG: water sources ====='
        print *,'Irrigation units IDs',irr_units%id
        print *,'Irrigation units Qnom',irr_units%q_nom
        print *,'Number of cells irrigable each day',irr_units%n_irrigable_cells
        print *,'===== END DEBUG ====='
    end if
end subroutine nom_water_supply

subroutine read_water_sources(year_length,pars,src_info)
    ! read daily discharges for one year
   integer,intent(in)::year_length
    type(source_info),intent(inout)::src_info
    type(parameters),intent(in)::pars

    if(pars%ms_i%f_exists .eqv. .true.) &
        & call read_daily_discharge_table(year_length,src_info%mn_src_tbl1,pars%ms_i)        ! monitored sources (i)
    if(pars%ms_ii%f_exists .eqv. .true.) &
        & call read_daily_discharge_table(year_length,src_info%mn_src_tbl2,pars%ms_ii)      ! monitored sources (ii)
    if(pars%intreu%f_exists .eqv. .true.) &
        & call read_daily_discharge_table(year_length,src_info%int_reuse_tbl,pars%intreu)    ! internal reuse

end subroutine read_water_sources

subroutine read_daily_discharge_table(year_length, mn_src_tbl, wat_src)
    ! read daily discharges tables for one year
    type(monitored_sources_table), intent(inout) :: mn_src_tbl
    integer,intent(in) :: year_length
    type(water_source),intent(in)::wat_src
    integer :: i

    if(wat_src%f_allocate_timeserie .eqv. .true.)then
        allocate(mn_src_tbl%q_daily(year_length,wat_src%n_withdrawals))
        ! read al the data
        do i=1,size(mn_src_tbl%q_daily,1)
            read(mn_src_tbl%unit, *) mn_src_tbl%q_daily(i,:)
        end do
    end if

end subroutine read_daily_discharge_table

subroutine destroy_water_sources_duty(src_info)

    type(source_info),intent(inout)::src_info

    if(associated(src_info%mn_src_tbl1%q_daily)) deallocate(src_info%mn_src_tbl1%q_daily)
    if(associated(src_info%mn_src_tbl2%q_daily)) deallocate(src_info%mn_src_tbl2%q_daily)
    if(associated(src_info%int_reuse_tbl%q_daily)) deallocate(src_info%int_reuse_tbl%q_daily)

end subroutine destroy_water_sources_duty

end module cli_watsources
