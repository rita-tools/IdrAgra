module mod_date
implicit none

! Calendar date and its derived calendar coordinates.
type date
    integer :: day = 0
    integer :: month = 0
    integer :: year = 0
    integer :: doy = 0       ! calendar day of year [1, 365/366]
    integer :: weekday = 0   ! Monday = 1, ..., Sunday = 7
end type date

private calendar_day_number

interface split_date
    module procedure split_date_range, parse_date
end interface

contains

! Return the number of days in each calendar month for the supplied year.
pure function month_lengths(year) result(days_in_month)
    integer, intent(in) :: year
    integer, dimension(12) :: days_in_month

    if (mod(year, 400) == 0 .or. (mod(year, 4) == 0 .and. mod(year, 100) /= 0)) then
        days_in_month = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    else
        days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    end if
end function month_lengths

! Return the number of days in the supplied calendar year.
pure function days_in_year(year) result(n_days)
    integer, intent(in) :: year
    integer :: n_days

    n_days = sum(month_lengths(year))
end function days_in_year

! Return the calendar day of year [1, 365/366].
pure function get_doy(day, month, year) result(doy)
    integer, intent(in) :: day, month, year
    integer :: doy
    integer, dimension(12) :: days_in_month

    days_in_month = month_lengths(year)
    doy = day + sum(days_in_month(:month-1))
end function get_doy

! Build a complete calendar date from its year and day of year.
pure function calendar_date_from_doy(year, doy) result(calendar_date)
    integer, intent(in) :: year, doy
    type(date) :: calendar_date

    integer :: remaining, month, days_in_month(12)

    days_in_month = month_lengths(year)
    remaining = max(1, doy)
    do month = 1, 12
        if (remaining <= days_in_month(month)) exit
        remaining = remaining - days_in_month(month)
    end do
    calendar_date%year = year
    calendar_date%month = month
    calendar_date%day = remaining
    calendar_date%doy = doy
    calendar_date%weekday = day_of_week(remaining, month, year)
end function calendar_date_from_doy

! Return the signed number of days from start_date to end_date.
pure function days_between_dates(start_date, end_date) result(n_days)
    type(date), intent(in) :: start_date, end_date
    integer :: n_days

    n_days = calendar_day_number(end_date%day, end_date%month, end_date%year) - &
           & calendar_day_number(start_date%day, start_date%month, start_date%year)
end function days_between_dates

! Compare complete calendar dates, independently from their derived fields.
elemental function dates_are_equal(first_date, second_date) result(are_equal)
    type(date), intent(in) :: first_date, second_date
    logical :: are_equal

    are_equal = first_date%year == second_date%year .and. &
              & first_date%month == second_date%month .and. &
              & first_date%day == second_date%day
end function dates_are_equal

! Return true when first_date occurs before second_date.
elemental function date_is_before(first_date, second_date) result(is_before)
    type(date), intent(in) :: first_date, second_date
    logical :: is_before

    is_before = first_date%year < second_date%year .or. &
              & (first_date%year == second_date%year .and. first_date%month < second_date%month) .or. &
              & (first_date%year == second_date%year .and. first_date%month == second_date%month .and. &
              &  first_date%day < second_date%day)
end function date_is_before

! Advance a calendar date by one day and keep its derived fields synchronized.
subroutine advance_calendar_date(calendar_date)
    type(date), intent(inout) :: calendar_date
    integer, dimension(12) :: days_in_month

    days_in_month = month_lengths(calendar_date%year)

    calendar_date%day = calendar_date%day + 1
    calendar_date%doy = calendar_date%doy + 1
    calendar_date%weekday = modulo(calendar_date%weekday, 7) + 1

    if (calendar_date%day > days_in_month(calendar_date%month)) then
        calendar_date%day = 1
        calendar_date%month = calendar_date%month + 1

        if (calendar_date%month > 12) then
            calendar_date%month = 1
            calendar_date%year = calendar_date%year + 1
            calendar_date%doy = 1
        end if
    end if
end subroutine advance_calendar_date

! Return the last day of the annual period beginning on start_date.
function annual_period_end(start_date) result(end_date)
    type(date), intent(in) :: start_date
    type(date) :: end_date
    integer, dimension(12) :: days_in_month

    end_date = start_date
    end_date%year = start_date%year + 1
    end_date%day = start_date%day - 1

    if (end_date%day == 0) then
        end_date%month = start_date%month - 1
        if (end_date%month == 0) then
            end_date%month = 12
            end_date%year = end_date%year - 1
        end if
        days_in_month = month_lengths(end_date%year)
        end_date%day = days_in_month(end_date%month)
    end if

    end_date%doy = get_doy(end_date%day, end_date%month, end_date%year)
    end_date%weekday = day_of_week(end_date%day, end_date%month, end_date%year)
end function annual_period_end

! Format a calendar date as dd/mm/yyyy.
function date_to_string(calendar_date) result(string)
    type(date), intent(in) :: calendar_date
    character(len=10) :: string

    write(string, '(I2.2,"/",I2.2,"/",I4.4)') calendar_date%day, calendar_date%month, calendar_date%year
end function date_to_string

! Count elapsed Gregorian calendar days before and within the supplied date.
pure function calendar_day_number(day, month, year) result(day_number)
    integer, intent(in) :: day, month, year
    integer :: day_number, previous_year

    previous_year = year - 1
    day_number = 365*previous_year + previous_year/4 - previous_year/100 + previous_year/400 + get_doy(day, month, year)
end function calendar_day_number

! Calculate the day of the week [1 = Monday, ..., 7 = Sunday].
pure function day_of_week(day, month, year) result(weekday)
    integer, intent(in) :: day, month, year
    integer :: weekday, zeller_day, century, year_in_century, adjusted_month, adjusted_year

    if (month <= 2) then
        adjusted_month = month + 12
        adjusted_year = year - 1
    else
        adjusted_month = month
        adjusted_year = year
    end if

    century = adjusted_year/100
    year_in_century = mod(adjusted_year, 100)
    zeller_day = mod(day + (adjusted_month+1)*26/10 + year_in_century + year_in_century/4 + century/4 + 5*century, 7) ![0 = Saturday, ..., 6 = Friday]
    weekday = modulo(zeller_day + 5, 7) + 1 ![1 = Monday, ..., 7 = Sunday]
end function day_of_week

! Split a date range formatted as "dd/mm/yyyy -> dd/mm/yyyy".
subroutine split_date_range(input_string, start_string, end_string)
    character(len=*), intent(in) :: input_string
    character(len=300), intent(out) :: start_string, end_string
    character(len=300) :: string
    character(len=2), parameter :: delimiter = '->'
    integer :: index

    string = trim(input_string)
    index = scan(string, delimiter)
    if (index == 0) then
        print *, 'Input files do not list end date or the delimiter "->" is not used'
        print *, 'Execution will be aborted...'
        stop
    end if
    start_string = string(1:index-1)
    index = scan(string, delimiter, .true.)
    end_string = string(index+1:)
end subroutine split_date_range

! Parse a day/month pair without a year, allowing 29 February for leap years.
subroutine parse_day_month_date(value, day, month, ios)
    character(len=*), intent(in) :: value
    integer, intent(out) :: day, month, ios

    integer :: separator, days_in_month(12)

    ios = 1
    separator = index(trim(value), '/')
    if (separator < 2) return
    read(value(:separator-1), *, iostat=ios) day
    if (ios /= 0) return
    read(value(separator+1:), *, iostat=ios) month
    if (ios /= 0) return
    ios = 1
    if (month < 1 .or. month > 12) return
    days_in_month = month_lengths(2000)
    if (day < 1 .or. day > days_in_month(month)) return
    ios = 0
end subroutine parse_day_month_date

! Parse a date formatted as "dd/mm/yyyy" and populate its derived fields.
subroutine parse_date(input_string, output_date)
    character(len=*), intent(in) :: input_string
    type(date), intent(out) :: output_date
    character(len=1), parameter :: delimiter = '/'
    character(len=300) :: string
    character(len=4) :: date_number
    integer :: index

    string = trim(input_string)
    index = scan(string, delimiter)
    if (index == 0) then
        print *, 'Input files are not correctly formatted'
        print *, 'Right date format is dd/mm/yyyy'
        stop 'Execution will be aborted...'
    end if
    date_number = adjustl(string(1:index-1))
    read(date_number, '(i2)') output_date%day

    string = string(index+1:)
    index = scan(string, delimiter)
    if (index == 0) then
        print *, 'Input files are not correctly formatted'
        print *, 'Right date format is dd/mm/yyyy'
        stop 'Execution will be aborted...'
    end if
    date_number = string(1:index-1)
    read(date_number, '(i2)') output_date%month
    date_number = string(index+1:)
    read(date_number, '(i4)') output_date%year

    ! TODO: add control to check date validity
    output_date%doy = get_doy(output_date%day, output_date%month, output_date%year)
    output_date%weekday = day_of_week(output_date%day, output_date%month, output_date%year)
end subroutine parse_date

end module mod_date
