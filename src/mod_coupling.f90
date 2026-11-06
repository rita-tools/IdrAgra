module mod_coupling
use, intrinsic :: iso_fortran_env, only: input_unit, output_unit
use mod_constants, only: dp
use mod_grid, only: bound, grid_i, grid_r, print_mat_as_grid, read_grid
use mod_parameters, only: simulation
use mod_system, only: delimiter
implicit none

private
public :: mf_coupling_state, init_mf_coupling, accumulate_mf_fluxes, coupling_is_due, coupling_handshake

character(len=*), parameter :: ready_token = 'IDRAGRA_COUPLING_READY'
character(len=*), parameter :: continue_token = 'IDRAGRA_COUPLING_CONTINUE'

type mf_coupling_state
    logical :: enabled = .false.
    integer :: period_days = 0
    integer :: period_index = 0
    integer :: days_in_period = 0
    character(len=500) :: directory = ''
    real(dp), dimension(:,:), allocatable :: net_percolation
    real(dp), dimension(:,:), allocatable :: private_pumping
end type mf_coupling_state

contains

subroutine init_mf_coupling(coupling, domain)
    type(mf_coupling_state), intent(out) :: coupling
    type(grid_i), intent(in) :: domain
    character(len=32) :: days_text
    integer :: env_length, env_status, ios

    ! Get the path to the coupling directory used to exchange fluxes with MODFLOW
    call get_environment_variable('IDRAGRA_COUPLING_DIR', coupling%directory, length=env_length, status=env_status)
    if (env_status /= 0 .or. env_length <= 0) then
        stop 'UseModflowCoupling requires launching IdrAgra through the MODFLOW coupling interface.'
    end if

    ! Make sure that the path ends with the correct delimiter
    if (coupling%directory(len_trim(coupling%directory):len_trim(coupling%directory)) /= '/' .and. &
        coupling%directory(len_trim(coupling%directory):len_trim(coupling%directory)) /= achar(92)) then
        coupling%directory = trim(coupling%directory)//delimiter
    end if

    call get_environment_variable('IDRAGRA_COUPLING_DAYS', days_text, status=env_status)
    read(days_text, *, iostat=ios) coupling%period_days
    if (ios /= 0 .or. coupling%period_days <= 0) then
        stop 'IDRAGRA_COUPLING_DAYS must be a positive integer.'
    end if

    allocate(coupling%net_percolation(size(domain%mat, 1), size(domain%mat, 2)))
    allocate(coupling%private_pumping(size(domain%mat, 1), size(domain%mat, 2)))
    coupling%net_percolation = 0.0_dp
    coupling%private_pumping = 0.0_dp
    coupling%enabled = .true.

    print *, 'IdrAgra/MODFLOW coupling enabled; period length [days]: ', coupling%period_days
end subroutine init_mf_coupling

subroutine accumulate_mf_fluxes(coupling, deep_percolation, capillary_rise, private_pumping)
    type(mf_coupling_state), intent(inout) :: coupling
    real(dp), dimension(:,:), intent(in) :: deep_percolation, capillary_rise, private_pumping

    coupling%net_percolation = coupling%net_percolation + deep_percolation - capillary_rise
    coupling%private_pumping = coupling%private_pumping + private_pumping
    coupling%days_in_period = coupling%days_in_period + 1
end subroutine accumulate_mf_fluxes

logical function coupling_is_due(coupling, is_last_day)
    type(mf_coupling_state), intent(in) :: coupling
    logical, intent(in) :: is_last_day

    coupling_is_due = coupling%days_in_period == coupling%period_days .or. is_last_day
end function coupling_is_due

subroutine coupling_handshake(coupling, domain, sim, extent, water_table, minimum_depth)
    type(mf_coupling_state), intent(inout) :: coupling
    type(grid_i), intent(in) :: domain
    type(simulation), intent(in) :: sim
    type(bound), intent(in) :: extent
    type(grid_r), intent(inout) :: water_table
    real(dp), intent(in) :: minimum_depth

    type(grid_r) :: returned_water_table
    real(dp), dimension(size(domain%mat, 1), size(domain%mat, 2)) :: output_matrix
    character(len=500) :: net_perc_path, pumping_path, water_table_path
    character(len=64) :: response_from_coupler, token
    character(len=6) :: period_text
    integer :: received_index, ios

    ! Write the exchange fluxes with the water table to files located in the coupling directory
    coupling%period_index = coupling%period_index + 1
    write(period_text, '(i6.6)') coupling%period_index
    net_perc_path = trim(coupling%directory)//'net_percolation_'//period_text//'.asc'
    pumping_path = trim(coupling%directory)//'pumping_'//period_text//'.asc'
    water_table_path = trim(coupling%directory)//'water_table_'//period_text//'.asc'

    output_matrix = coupling%net_percolation
    where (domain%mat == domain%header%nan) output_matrix = real(domain%header%nan, dp)
    call print_mat_as_grid(trim(net_perc_path), domain%header, output_matrix)

    output_matrix = coupling%private_pumping
    where (domain%mat == domain%header%nan) output_matrix = real(domain%header%nan, dp)
    call print_mat_as_grid(trim(pumping_path), domain%header, output_matrix)

    ! Actual handshake with the coupler: after reading this line, the coupler allows MODFLOW to progress
    write(output_unit, '(a,1x,i0,1x,i0)') ready_token, coupling%period_index, coupling%days_in_period
    flush(output_unit)

    ! Wait for MODFLOW to run and for the coupler to send a CONTINUE command with the correct period index
    read(input_unit, '(A)', iostat=ios) response_from_coupler
    if (ios /= 0) stop 'Coupling controller closed stdin before sending CONTINUE.'
    read(response_from_coupler, *, iostat=ios) token, received_index
    if (ios /= 0 .or. trim(token) /= continue_token .or. received_index /= coupling%period_index) then
        print *, 'Invalid coupling response: ', trim(response_from_coupler)
        stop 'IdrAgra/MODFLOW coupling protocol error.'
    end if

    ! Read the water-table grid returned by MODFLOW
    call read_grid(trim(water_table_path), returned_water_table, sim, extent)
    if (any(shape(returned_water_table%mat) /= shape(water_table%mat))) then
        stop 'Returned MODFLOW water-table grid has the wrong shape.'
    end if
    water_table%mat = returned_water_table%mat
    where (water_table%mat < minimum_depth .and. water_table%mat /= water_table%header%nan)
        water_table%mat = minimum_depth
    end where
    deallocate(returned_water_table%mat)

    ! Reset coupling state
    coupling%net_percolation = 0.0_dp
    coupling%private_pumping = 0.0_dp
    coupling%days_in_period = 0
end subroutine coupling_handshake

end module mod_coupling
