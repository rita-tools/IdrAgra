module mod_meteo
use mod_constants, only: dp
use mod_date, only: date, days_in_year, split_date, days_between_dates, dates_are_equal, date_is_before
use mod_utility, only: lower_case
use mod_parameters, only: simulation, par_method
use mod_evapotranspiration, only: ET_reference
use mod_grid, only: grid_i
implicit none

! store weather station data
type meteo_info
    integer::unit                   ! unit associated to the input file
    integer::station_id             ! stable ID read from the weather-file header
    character(len=255)::filename    ! name of the input file
    real(dp)::x_m                   ! longitude
    real(dp)::y_m                   ! latitude
    type(date)::start               ! first day of the time serie
    type(date)::finish              ! last day of the time serie
    real(dp)::alt_m                 ! altitude [m]
    real(dp)::lat_deg               ! latitude [degree]
    real(dp)::T_max                 ! maximum daily temperature [°C]
    real(dp)::T_min                 ! minimum daily temperature [°C]
    real(dp)::P                     ! daily precipitation [mm]
    real(dp)::P_cum                 ! cumulative precipitation from the next forecast_day days [mm] (currently unused)
    real(dp)::RH_max                ! maximum volumetric air moisture [%]
    real(dp)::RH_min                ! minimum volumetric air moisture [%]
    real(dp)::wind_vel              ! wind velocity at 2 m height [m/s]
    real(dp)::sol_rad               ! daily solar radiation [MJ m-2 day-1]
    real(dp)::et0                   ! reference et0
end type meteo_info

! It contains the weather parameters spatially distributed
! The distribution is made over the domain with the weights defined in info_spat%weigth_ws(:)
type meteo_mat
    real(dp), dimension(:,:), allocatable :: T_ave    ! Daily average temperature [°C]
    real(dp), dimension(:,:), allocatable :: T_max    ! maximum daily temperature [°C]
    real(dp), dimension(:,:), allocatable :: T_min    ! minimum daily temperature [°C]
    real(dp), dimension(:,:), allocatable :: P        ! precipitation [mm]
    real(dp), dimension(:,:), allocatable :: P_cum    ! cumulate precipitation [mm]
    real(dp), dimension(:,:), allocatable :: RH_max   ! maximum daily air moisture [%]
    real(dp), dimension(:,:), allocatable :: RH_min   ! minimum daily air moisture [%]
    real(dp), dimension(:,:), allocatable :: Wind_vel ! wind velocity at 2 m height [m/s]
    real(dp), dimension(:,:), allocatable :: Rad_sol  ! global solar radiation [MJ m^-2 d^-1]
    real(dp), dimension(:,:), allocatable :: lat      ! Latitude [m]
    real(dp), dimension(:,:), allocatable :: alt      ! Altitude [m a.s.l.]
    real(dp), dimension(:,:), allocatable :: et0      ! evapotranspiration [mm]
end type meteo_mat

contains

subroutine meteo_series_length(sim, verbose)
    ! calculate the length of the weather time series [days]
    ! check if time series have the same length
    type(simulation),intent(inout)::sim
    logical,optional,intent(in)::verbose
    type(meteo_info),dimension(:),allocatable::info_meteo
    integer::k
    real(dp)::value
    integer::count,gg_count,gg_in_yy, gg_diff

    call read_meteo_parameters(sim,info_meteo,verbose)

    ! Set simulation dates, if not already set
    if (sim%start%year == 0) sim%start = info_meteo(1)%start
    if (sim%end%year == 0) sim%end = info_meteo(1)%finish

    ! Verify that meteorological series are coherent
    if (any(.not. dates_are_equal(info_meteo(:)%start, info_meteo(1)%start))) then
        stop 'Meteorological series start in different days. Execution will be aborted...'
    end if
    if (any(.not. dates_are_equal(info_meteo(:)%finish, info_meteo(1)%finish))) then
        stop 'Meteorological series end in different days. Execution will be aborted...'
    end if

    ! Verify that meteorological series are coherent with simulation dates
    if (any(date_is_before(sim%start, info_meteo(:)%start))) then
        stop 'Meteorological series are not coherent with simulation dates. Execution will be aborted...'
    end if
    if (any(date_is_before(info_meteo(:)%finish, sim%end))) then
        stop 'Meteorological series are not coherent with simulation dates. Execution will be aborted...'
    end if

    ! Check length of time series
    do k = 1,sim%n_weather_stations
        gg_diff = days_between_dates(info_meteo(k)%start, info_meteo(k)%finish) + 1
        count=0
        do while (.true.)
            read (info_meteo(k)%unit, *, end=999) value
            count=count+1
        end do
        999 continue
        if (count /= gg_diff) then
            print *,"Meteorological series have incoherent lengths"
            print *,"The range between dates is ", gg_diff, " while data series is of ", count, " elements"
            print *, 'Execution will be aborted...'
            stop
        end if

        if(k==1)then
            gg_count=count
        else
            if(count/=gg_count)then
                stop 'Meteorological series have different lengths. Execution will be aborted...'
            end if
        end if
    end do

    ! calculate the number of years in the time series
    sim%meteo_years=0; gg_in_yy=0
    sim%start_year = info_meteo(1)%start%year ! first year
    do
        sim%meteo_years=sim%meteo_years+1
        gg_in_yy = gg_in_yy + days_in_year(sim%start_year + sim%meteo_years - 1)
        if(gg_in_yy>=gg_count) exit
    end do
    sim%sim_years=sim%meteo_years

    ! Updating gg_count to take into account the end of simulation date
    if (date_is_before(sim%end, info_meteo(1)%finish)) then
        gg_count = days_between_dates(info_meteo(1)%start, sim%end) + 1
        ! calculate the number of years to be simulated
        sim%sim_years=0; gg_in_yy=0
        sim%start_year = info_meteo(1)%start%year ! first year
        do
            sim%sim_years=sim%sim_years+1
            gg_in_yy = gg_in_yy + days_in_year(sim%start_year + sim%sim_years - 1)
            if(gg_in_yy>=gg_count) exit
        end do
    end if

    allocate(sim%days_in_year(sim%sim_years))

    sim%days_in_year=0

    ! calculate the number of days for each simulation years
    do k=1,sim%sim_years
        if(k/=sim%sim_years)then
            if (info_meteo(1)%start%month > 2) then
                sim%days_in_year(k) = days_in_year(sim%start_year + k)
            else
                sim%days_in_year(k) = days_in_year(sim%start_year + k - 1)
            end if
        else ! the last year fo the dataset could be uncompleted
            sim%days_in_year(k)=gg_count-sum(sim%days_in_year)
        end if
    end do
    call close_meteo_file(info_meteo)

    !debug
    if(verbose .eqv. .true.)then
        print *,'===== DEBUG: meteo series length ====='
        print*,"Total number of days: ",gg_count
        print*,"First simulation year: ",sim%start_year
        print*,"Simulation length: ",sim%sim_years, " years"
        print*,"Number of days for each year:"
        do k=1,sim%sim_years
            print*," ",sim%days_in_year(k),"<---",sim%start_year+k-1
        end do
        print *,'===== END DEBUG ====='
    end if
end subroutine meteo_series_length

subroutine read_meteo_parameters(sim,info_meteo,verbose)
    ! read the number  of weather station in the file "weather_stations.dat" and init the array
    ! for each station, it will be read: the name of the station, the latitude, the altitude, the starting day of the time series
   type(simulation),intent(inout)::sim
    type(meteo_info),dimension(:),allocatable,intent(inout)::info_meteo
    logical, intent(in)::verbose

    integer :: free_unit, i
    integer :: ios
    character(len=255) :: filemeteo_name
    integer :: n_stations
    character(len=255)::dir
    character(len=300) :: comment,buffer, label
    integer :: p
    integer :: line, tablestart
    character(len=300) :: date_string, station_header
    character(len=300) :: date_start, date_end
    integer :: header_station_id
    logical :: station_id_valid, station_id_mismatch

    ios = 0
    line = 0; tablestart = 0
    dir=trim(sim%meteo_path)
    filemeteo_name=trim(sim%ws_list_fn)

    open(newunit=free_unit, file=filemeteo_name, status='old', action="read", iostat=ios)
    if (ios /= 0 ) then
        print *, 'Cannot open file ', filemeteo_name, '. The specified file does not exist. Execution will be aborted...'
        stop
    end if

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
                    case ('statnum')
                        read(buffer, *, iostat=ios) n_stations ! number of weather station
                        if (n_stations /= sim%n_weather_stations) then
                            print *, "Meteorological stations number (MeteoStatTotNum) in simulation parameter &
                                & file is not equal to the number (StatNum) in ", filemeteo_name, " database"
                            print *, 'Execution will be aborted...'
                            stop
                        end if
                        allocate(info_meteo(sim%n_weather_stations))    ! allocate enough memory to store weather stations information
                    case ('table')
                        tablestart = line
                        read (free_unit, *); line = line + 1 ! skip the header of the table
                        do i=1,size(info_meteo)
                            read(free_unit,*)info_meteo(i)%filename, info_meteo(i)%x_m, info_meteo(i)%y_m
                            line = line + 1
                            open(newunit=info_meteo(i)%unit, file=trim(dir)//trim(info_meteo(i)%filename), status='old',&
                                & action="read", iostat=ios )
                            if (ios /= 0 ) then
                                print *, 'Cannot open file ', trim(info_meteo(i)%filename), '. The specified file does &
                                    & not exist. Execution will be aborted...'
                                stop
                            end if

                            !%PS%
                            ! Read the station ID, either from:
                            !   - the filename (if numeric)
                            !   - the first line of the file (expected format: "Id station: <x>, location: <a string>")
                            read(info_meteo(i)%unit, '(a300)', iostat=ios) station_header
                            if (ios /= 0) then
                                print *, 'Cannot read the first line of ', trim(info_meteo(i)%filename), '.'
                                print *, 'Execution will be aborted...'
                                stop
                            end if
                            call resolve_weather_station_id(info_meteo(i)%filename, station_header, &
                                & info_meteo(i)%station_id, header_station_id, station_id_valid, station_id_mismatch)
                            if (.not. station_id_valid) then
                                print *, 'Invalid weather-station ID in ', trim(info_meteo(i)%filename), &
                                    & ': ', trim(station_header)
                                print *, 'A valid positive header ID is required when the filename is not numeric.'
                                print *, 'Execution will be aborted...'
                                stop
                            end if
                            if (verbose .and. station_id_mismatch) then
                                print *, 'Warning: station ID ', header_station_id, ' in ', &
                                    & trim(info_meteo(i)%filename), ' differs from numeric filename ID ', &
                                    & info_meteo(i)%station_id, '; using the filename ID.'
                            end if

                            read(info_meteo(i)%unit,*)  info_meteo(i)%lat_deg, info_meteo(i)%alt_m ! read latitude and altitude
                            ! Read the first and the last days, separated by " -> " : e.g. "01/01/1993 -> 31/12/2014" TODO: reverse the order aaaa/mm/dd
                            read(info_meteo(i)%unit, '(a300)') date_string
                            call split_date(date_string, date_start, date_end)
                            call split_date(date_start, info_meteo(i)%start)
                            call split_date(date_end, info_meteo(i)%finish)
                            read(info_meteo(i)%unit,*)  ! skip line
                        end do
                    case ('endtable')
                        if ((line - tablestart - 2) < n_stations) then
                            stop 'Meteorological stations number (StatNum) is higher than meteorological &
                                & station listed in the table. Execution will be aborted...'
                        else if ((line - tablestart - 2) > n_stations) then
                            stop 'Meteorological stations number (StatNum) is lower than meteorological &
                                & station listed in the table. Execution will be aborted...'
                        end if
                    case default ! all other cases ...
                        print *, 'Skipping invalid label <',trim(label),'> at line', line, ' of file: ', trim(filemeteo_name) ! todo: line number here is not reliable when size(info_meteo) doesn't match the actual number of entries
                        if (trim(label) /= '') print *, 'labels (StatNum, Table, EndTable) must be followed by an equal sign (=)'
                        print *, 'Execution will be aborted...'
                        stop
                end select
            end if
        end if
    end do
    close(free_unit)

    ! Check for duplicated IDs
    do i=1,size(info_meteo)
        if (count(info_meteo%station_id == info_meteo(i)%station_id) > 1) then
            print *, 'Duplicate weather-station ID ', info_meteo(i)%station_id, &
                & ' found among the resolved meteorological station identifiers.'
            print *, 'Execution will be aborted...'
            stop
        end if
    end do

    if (verbose .eqv. .true.) then
        print *,'===== DEBUG: weather station data====='
        print *,'Weather station file: ',  filemeteo_name
        print *,'Meteo files path: ',  dir
        print *,'# Weather station in list file: ',  size(info_meteo)
        print *,'id name lon_m lat_m lat_g alt_m dd/mm/yy'
        do i=1,size(info_meteo)
            print *, info_meteo(i)%station_id, ' ', trim(info_meteo(i)%filename),' ' , &
                info_meteo(i)%x_m,' ', info_meteo(i)%y_m, ' ', &
                info_meteo(i)%lat_deg, ' ', info_meteo(i)%alt_m, ' ',  &
                info_meteo(i)%start%day, '/', info_meteo(i)%start%month, '/', info_meteo(i)%start%year
        end do
        print *,'===== END DEBUG ====='
    end if

end subroutine read_meteo_parameters

subroutine close_meteo_file(info_meteo)
    ! close all files with weather data and set memory free
   type(meteo_info),dimension(:),allocatable,intent(inout)::info_meteo

    integer::i

    do i=1,size(info_meteo)
        close(info_meteo(i)%unit)
    end do
    deallocate(info_meteo)
end subroutine close_meteo_file

! Advance every weather-station file read skipping n_days records.
subroutine skip_meteo_days(info_meteo, n_days)
    type(meteo_info), dimension(:), intent(inout) :: info_meteo
    integer, intent(in) :: n_days
    integer :: station_idx, day_idx
    real(dp) :: unused_value

    do station_idx = 1, size(info_meteo)
        do day_idx = 1, n_days
            read(info_meteo(station_idx)%unit, *) unused_value
        end do
    end do
end subroutine skip_meteo_days

subroutine read_meteo_data(info_meteo,current_doy,res_surf, forecast_day)
    ! read meteo data and update ET0
   integer,intent(in)::current_doy
    integer,intent(in)::forecast_day
    type(meteo_info),dimension(:),intent(inout)::info_meteo
    real(dp),intent(in)::res_surf ! surface resistance
    real(dp) :: dummy(3) = 0.0D0

    integer::i, ii
    integer::IOstatus

    do i=1,size(info_meteo)
        read(info_meteo(i)%unit,*,iostat=IOstatus) &
            info_meteo(i)%T_max,info_meteo(i)%T_min,info_meteo(i)%P,info_meteo(i)%RH_max, &
            info_meteo(i)%RH_min,info_meteo(i)%wind_vel,info_meteo(i)%sol_rad
        ! init and populate cumulate precipitation
        info_meteo(i)%P_cum = info_meteo(i)%P
        do ii=1,forecast_day
            read(info_meteo(i)%unit,*,iostat=IOstatus) dummy
            info_meteo(i)%P_cum = info_meteo(i)%P_cum + dummy(3)
        end do
        ! go back
        do ii=1,forecast_day
            backspace(info_meteo(i)%unit,iostat=IOstatus)
        end do

    end do

    ! calculate ET0
    do i=1,size(info_meteo)
        info_meteo(i)%et0=ET_reference(info_meteo(i)%T_max,info_meteo(i)%T_min,info_meteo(i)%RH_max, &
                                        info_meteo(i)%RH_min,info_meteo(i)%wind_vel,info_meteo(i)%sol_rad,&
                                        info_meteo(i)%lat_deg,info_meteo(i)%alt_m,res_surf,current_doy)
    end do
end subroutine read_meteo_data

!%PS%: Weather station filenames can now be either numbers or strings.
!     - If a number: station's ID is the filename
!     - If a string: station's ID is read from the first line of the file
subroutine resolve_weather_station_id(filename, station_header, station_id, id_from_header, id_valid, id_mismatch)
    character(len=*), intent(in) :: filename, station_header
    integer, intent(out) :: station_id, id_from_header
    logical, intent(out) :: id_valid, id_mismatch
    character(len=255) :: filename_stem
    integer :: colon_pos, comma_pos, extension_pos, id_from_filename, ios
    logical :: header_id_valid, numeric_filename

    id_from_header = 0
    header_id_valid = .false.
    colon_pos = index(station_header, ':')
    comma_pos = index(station_header, ',')
    if (colon_pos > 0 .and. comma_pos > colon_pos+1) then
        read(station_header(colon_pos+1:comma_pos-1), *, iostat=ios) id_from_header
        header_id_valid = ios == 0 .and. id_from_header > 0
    end if

    filename_stem = trim(adjustl(filename))
    extension_pos = index(trim(filename_stem), '.')
    if (extension_pos > 1) filename_stem = filename_stem(:extension_pos-1)

    ! Check if the weather station's filename is a number, in that case interpret it as the station's ID
    numeric_filename = len_trim(filename_stem) > 0 .and. verify(trim(filename_stem), '0123456789') == 0
    id_from_filename = 0
    if (numeric_filename) then
        read(filename_stem, *, iostat=ios) id_from_filename
        numeric_filename = ios == 0 .and. id_from_filename > 0
    end if

    if (numeric_filename) then ! Station's ID is the filename (without extension)
        station_id = id_from_filename
        id_valid = .true.
        id_mismatch = header_id_valid .and. id_from_header /= id_from_filename
    else                       ! Station's ID is read from the first line of the file
        station_id = id_from_header
        id_valid = header_id_valid
        id_mismatch = .false.
    end if
end subroutine resolve_weather_station_id

subroutine create_meteo_matrices(info_meteo, dir_meteo, meteo_weight, meteo, domain, doy, res_canopy, sim)
    ! Distribute weather variables using station weights or the closest station.
    type(meteo_info), dimension(:), intent(in) :: info_meteo
    integer, dimension(:,:,:), intent(in) :: dir_meteo
    type(meteo_mat), intent(inout) :: meteo
    real(dp), dimension(:,:,:), intent(in) :: meteo_weight
    type(grid_i), intent(in) :: domain
    integer, intent(in) :: doy
    real(dp), intent(in) :: res_canopy
    type(simulation),intent(in) :: sim
    integer :: i, j, k, station
    real(dp) :: weight

    call reset_meteo(meteo)

    ! For interpolated variables
    do k=1,size(meteo_weight,3)
        do j=1,size(domain%mat,2)
            do i=1,size(domain%mat,1)
                if (domain%mat(i,j)/=domain%header%nan) then
                    station = dir_meteo(i,j,k)
                    weight = meteo_weight(i,j,k)
                    if (sim%interpolate_temp) then
                        meteo%T_max(i,j) =    meteo%T_max(i,j) +    info_meteo(station)%T_max *    weight
                        meteo%T_min(i,j) =    meteo%T_min(i,j) +    info_meteo(station)%T_min *    weight
                    end if
                    if (sim%interpolate_rain) then
                        meteo%P(i,j) =        meteo%P(i,j) +        info_meteo(station)%P *        weight
                        meteo%P_cum(i,j) =    meteo%P_cum(i,j) +    info_meteo(station)%P_cum *    weight
                    end if
                    if (sim%interpolate_hum) then
                        meteo%RH_max(i,j) =   meteo%RH_max(i,j) +   info_meteo(station)%RH_max *   weight
                        meteo%RH_min(i,j) =   meteo%RH_min(i,j) +   info_meteo(station)%RH_min *   weight
                    end if
                    if (sim%interpolate_wind) then
                        meteo%Wind_vel(i,j) = meteo%Wind_vel(i,j) + info_meteo(station)%wind_vel * weight
                    end if
                    if (sim%interpolate_rad) then
                        meteo%Rad_sol(i,j) =  meteo%Rad_sol(i,j) +  info_meteo(station)%sol_rad *  weight
                    end if
                    meteo%lat(i,j) =          meteo%lat(i,j) +      info_meteo(station)%lat_deg *  weight
                    meteo%alt(i,j) =          meteo%alt(i,j) +      info_meteo(station)%alt_m *    weight
                end if
            end do
        end do
    end do

    ! For non-interpolated variables
    do j=1,size(domain%mat,2)
        do i=1,size(domain%mat,1)
            if (domain%mat(i,j)/=domain%header%nan) then
                station = dir_meteo(i,j,1) ! Use the closest station's data
                if (.not. sim%interpolate_temp) then
                    meteo%T_max(i,j) = info_meteo(station)%T_max
                    meteo%T_min(i,j) = info_meteo(station)%T_min
                end if
                if (.not. sim%interpolate_rain) then
                    meteo%P(i,j) = info_meteo(station)%P
                    meteo%P_cum(i,j) = info_meteo(station)%P_cum
                end if
                if (.not. sim%interpolate_hum) then
                    meteo%RH_max(i,j) = info_meteo(station)%RH_max
                    meteo%RH_min(i,j) = info_meteo(station)%RH_min
                end if
                if (.not. sim%interpolate_wind) meteo%Wind_vel(i,j) = info_meteo(station)%wind_vel
                if (.not. sim%interpolate_rad) meteo%Rad_sol(i,j) = info_meteo(station)%sol_rad
            end if
        end do
    end do

    ! calculate ET0 from distributed parameters
    meteo%et0 = ET_reference(meteo%T_max, meteo%T_min, meteo%RH_max, meteo%RH_min, meteo%Wind_vel, meteo%Rad_sol,&
                                   meteo%lat, meteo%alt, res_canopy, doy, domain%header%imax, domain%header%jmax)
end subroutine create_meteo_matrices

subroutine reset_meteo(meteo)
    type(meteo_mat), intent(inout) :: meteo
    ! Resets weather variables for each simulation cell (dimensions are 'row, column')
    meteo%T_max(:,:) = 0.0_dp
    meteo%T_min(:,:) = 0.0_dp
    meteo%P(:,:) = 0.0_dp
    meteo%P_cum(:,:) = 0.0_dp
    meteo%RH_max(:,:) = 0.0_dp
    meteo%RH_min(:,:) = 0.0_dp
    meteo%Wind_vel(:,:) = 0.0_dp
    meteo%Rad_sol(:,:) = 0.0_dp
    meteo%lat(:,:) = 0.0_dp
    meteo%alt(:,:) = 0.0_dp
    meteo%et0(:,:) = 0.0_dp
    meteo%T_ave(:,:) = 0.0_dp
end subroutine reset_meteo

end module mod_meteo
