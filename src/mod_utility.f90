module mod_utility

use mod_constants, only: dp, pi
implicit none

private get_value_index_i, get_value_index_c

interface get_value_index
    module procedure get_value_index_i, get_value_index_c
end interface

contains

! Remove comments and normalize whitespace in a text line
pure function clean_input_line(line) result(cleaned)
    character(len=*), intent(in) :: line
    character(len=len(line)) :: cleaned
    integer :: comment_idx, idx

    cleaned = line
    comment_idx = scan(cleaned, '#')
    if (comment_idx > 0) cleaned(comment_idx:) = ' '

    do idx = 1, len_trim(cleaned)
        if (cleaned(idx:idx) == achar(9)) cleaned(idx:idx) = ' '
    end do

    cleaned = adjustl(cleaned)
end function clean_input_line

pure function join_path(folder, file_name, separator) result(path)
    character(len=*), intent(in) :: folder, file_name
    character, intent(in) :: separator
    character(len=:), allocatable :: path
    integer :: folder_length

    folder_length = len_trim(folder)
    if (folder_length == 0) then
        path = trim(file_name)
    else if (folder(folder_length:folder_length) == '/' .or. &
           & folder(folder_length:folder_length) == achar(92)) then
        path = trim(folder)//trim(file_name)
    else
        path = trim(folder)//separator//trim(file_name)
    end if
end function join_path

! Integer-to-string conversion without leading or trailing spaces.
pure function itoa(value) result(string)
    integer, intent(in) :: value
    character(len=:), allocatable :: string
    character(len=32) :: buffer

    write(buffer, '(I0)') value
    string = trim(buffer)
end function itoa

! Count occurrences of each unique string, preserving first-seen order.
! e.g. ["a","a","b","a","c","c"] --> [3, 1, 2]
subroutine count_elements(strings, counts)
    character(len=*), dimension(:), intent(in) :: strings
    integer, dimension(:), allocatable, intent(out) :: counts

    integer :: i, n_unique_strings
    logical :: already_counted

    n_unique_strings = 0

    ! Determine the result size from the number of first occurrences.
    do i = 1, size(strings)
        already_counted = .false.
        if (i > 1) already_counted = any(strings(:i-1) == strings(i))
        if (already_counted) cycle

        n_unique_strings = n_unique_strings + 1
    end do

    allocate(counts(n_unique_strings))

    ! Store counts in the same first-seen order.
    n_unique_strings = 0
    do i = 1, size(strings)
        already_counted = .false.
        if (i > 1) already_counted = any(strings(:i-1) == strings(i))
        if (already_counted) cycle

        n_unique_strings = n_unique_strings + 1
        counts(n_unique_strings) = count(strings == strings(i))
    end do
end subroutine count_elements

subroutine split_string(string, delimiter, substrings, substring_count)
    ! split a string into two sides of a delimiter token
    character(len=*), intent(in) :: string
    character, intent(in) :: delimiter
    character(len=*), intent(out) :: substrings(*)
    integer, intent(out) :: substring_count

    integer :: start_position, end_position

    start_position = 1
    substring_count = 0

    do
        end_position = index(string(start_position:), delimiter)
        substring_count = substring_count +1
        if (end_position == 0) then
            substrings(substring_count) = string(start_position:)
            exit
        else
            substrings(substring_count) = string(start_position : start_position + end_position - 2)
            start_position = start_position + end_position
        end if
    end do
end subroutine split_string

subroutine lower_case(word)
    ! convert a word to lower case
    character (len=*) , intent(inout) :: word
    integer :: i,ic,nlen
    nlen = len(word)
    if (nlen == 0) stop "Zero characters string"
    do i=1,nlen
        ic = ichar(word(i:i))
        if (ic >= 65 .and. ic <= 90) word(i:i) = char(ic+32)
    end do
end subroutine lower_case

pure function get_value_index_i( val_list, a_value) result( val_idx)
    ! Find the position of an integer value in an array of integers
    integer, dimension(:), intent(in) :: val_list    ! a list of values
    integer, intent(in) :: a_value                   ! the value to be found
    integer :: val_idx                               ! the position in the array
    integer :: i                                     ! counter

    val_idx = 0 ! default value, value not found
    do i=1, size(val_list)
        if(val_list(i) == a_value) then
            val_idx = i
            return
        end if
    end do
end function get_value_index_i

pure function get_value_index_c( val_list, a_value) result( val_idx)
    ! Find the position of a character value in an array of characters
    character(len=*), dimension(:), intent(in) :: val_list  ! a list of values
    character(len=*), intent(in) :: a_value                 ! the value to be found
    integer :: val_idx                                      ! the position in the array
    integer :: i                                            ! counter

    val_idx = 0 ! Default value, value not found
    do i=1, size(val_list)
        if(val_list(i) == a_value) then
            val_idx = i
            return
        end if
    end do
end function get_value_index_c

subroutine calc_time_diff(t_start, t_stop, t_delta)
    ! Calculate the difference between two times
    ! TODO: difference in days not implemented

    integer, dimension(8), intent(in) :: t_start, t_stop
    integer, dimension(8), intent(out) :: t_delta
    integer :: milliseconds, seconds, minutes, hours, days

    t_delta = 0 ! init to zero

    milliseconds = t_stop(8) - t_start(8)
    seconds = t_stop(7) - t_start(7)
    minutes = t_stop(6) - t_start(6)
    hours = t_stop(5) - t_start(5)
    days = t_stop(3) - t_start(3)
    if( milliseconds < 0) then
        milliseconds = milliseconds + 1000
        seconds = seconds - 1
    end if
    if( seconds < 0 ) then
        seconds = seconds + 60
        minutes = minutes - 1
    end if
    if( minutes < 0 ) then
        minutes = minutes + 60
        hours = hours - 1
    end if
    if( hours < 0  ) then
        hours = hours + 24
        days = days - 1
        !per ora nulla
    end if
    t_delta(3) = days
    t_delta(5) = days*24+hours ! add hours of completed days
    t_delta(6) = minutes
    t_delta(7) = seconds
    t_delta(8) = milliseconds
end subroutine calc_time_diff

subroutine print_execution_time(t_start, t_stop)
    ! print the difference between time
    ! TODO: mode to cli
    integer, dimension(8), intent(in) :: t_start, t_stop
    integer, dimension(8) :: t_delta

    call calc_time_diff(t_start, t_stop, t_delta)
    print *, " ===> Simulation duration: ", t_delta(5), "h ", t_delta(6), "' ", t_delta(7), ' " '
end subroutine print_execution_time

pure function make_numbered_name(n,ext) result(num_name)
    ! Dalla stringa nome del file (espresso in numero), estensione, genera un nome di file completo
    integer, intent(in)::n              ! number of file
    character(len=4),intent(in)::ext    ! file extention
    character(len=30)::num_name         ! complete numbered name
    num_name = itoa(n)//trim(adjustl(ext))
end function make_numbered_name

subroutine get_uniform_sample(irandom, amplitude, rand_symmetry,repeatable)
    ! Get a matrix of random number from a matrix of integer values
    ! values are included between [-amplitude, +amplitude]
    integer,dimension(:,:),intent(out)::irandom
    integer,intent(in)::amplitude
    logical,intent(in)::rand_symmetry
    logical, intent(in) :: repeatable
    real(dp),dimension(size(irandom,1),size(irandom,2))::rrandom

    ! note about "random_init(repeatable ,image_distinct)"
    ! repeatable : is true, use the same initialization values
    ! image_distinct - mostly for coarray parallel programs, if true, each image has its random setup
    call random_init(repeatable, .false.)
    call random_number(rrandom)             ! generate a range [0-1]

    if (rand_symmetry .eqv. .true.) then
        ! TODO: check
        irandom=int(amplitude*(2*rrandom-1)) ! transform R[0,1] in iR [-n,+n] with n = amplitude (e.g. amplitude = 4, iR = 0,-9)
    else
        irandom=int(rrandom*(2.d0*amplitude+1)) ! transform R[0,1] in iR [0,2n+1] with n = amplitude (e.g. amplitude = 4, iR = 0,-9)
    end if

end subroutine get_uniform_sample

function string_to_integers(str, sep) result(a)
    ! return a sequence of integers from a string
    integer, allocatable :: a(:)
    character(*) :: str
    character :: sep
    integer :: i, n_sep

    n_sep = 0

    do i = 1, len(str)
      if (str(i:i)==sep) then
        n_sep = n_sep + 1
        str(i:i) = ','
       end if
    end do
    allocate(a(n_sep+1))
    read(str,*) a
end function string_to_integers

function string_to_reals(str, sep) result(a)
    ! return a sequence of reals from a string
    real(dp), allocatable :: a(:)
    character(*) :: str
    character :: sep
    integer :: i, n_sep

    n_sep = 0
    do i = 1, len(str)
      if (str(i:i)==sep) then
        n_sep = n_sep + 1
        str(i:i) = ','
       end if
    end do

    allocate(a(n_sep+1))
    read(str,*) a
end function string_to_reals

pure recursive function replace_str(string,search,substitute) result(modifiedString)
    ! https://stackoverflow.com/questions/58938347/how-do-i-replace-a-character-in-the-string-with-another-charater-in-fortran
    character(len=*), intent(in)  :: string, search, substitute
    character(len=:), allocatable :: modifiedString
    integer                       :: i, stringLen, searchLen
    stringLen = len(string)
    searchLen = len(search)
    if (stringLen==0 .or. searchLen==0) then
        modifiedString = ""
        return
    elseif (stringLen<searchLen) then
        modifiedString = string
        return
    end if
    i = 1
    do
        if (string(i:i+searchLen-1)==search) then
            modifiedString = string(1:i-1) // substitute // replace_str(string(i+searchLen:stringLen),search,substitute)
            exit
        end if
        if (i+searchLen>stringLen) then
            modifiedString = string
            exit
        end if
        i = i + 1
        cycle
    end do
end function replace_str

function round_2darray(mat, n) result(res)
    real(dp), dimension(:,:)::mat
    real(dp), allocatable ::res(:,:)
    integer :: n
    allocate(res(size(mat,1),size(mat,2)))
    res = anint(mat*10.0**n)/10.0**n
end function round_2darray

pure elemental function pdf_normal(x, x_mean, x_std) result(pdf)
    real(dp), intent(in):: x, x_mean, x_std
    real(dp):: pdf

    pdf = (1/((2*pi*x_std**2)**0.5))*exp(-((x-x_mean)**2)/(2*x_std**2))
end function

end module mod_utility
