module mod_crop_phenology
use mod_grid
use mod_utility, only: round_2darray
implicit none

type crop_definition
    character(len=255) :: crop_name = ''
    character(len=255) :: parameter_file = ''
    integer :: crop_id = 0

    ! Sowing, harvest and rotation constraints
    integer :: sowing_doy_min = 0
    integer :: sowing_delay_max = 0
    integer :: harvest_doy_max = 0
    integer :: max_cuts = 1
    integer :: crop_overlap_days = 0 ! Days of bare soil needed before this crop can be sown

    ! Thermal-time parameters
    real(dp) :: sowing_temp = 0._dp
    real(dp) :: base_temp = 0._dp
    real(dp) :: cutoff_temp = 0._dp

    ! Yield-stage boundaries in the uncorrected Kcb curve (accumulated GDD).
    ! Huge values mark stages absent from a crop definition.
    real(dp) :: emergence_gdd = huge(0._dp)
    real(dp) :: initial_end_gdd = huge(0._dp)
    real(dp) :: mid_start_gdd = huge(0._dp)
    real(dp) :: mid_end_gdd = huge(0._dp)

    ! Vernalization parameters
    logical :: requires_vernalization = .false.
    real(dp) :: vern_temp_min = 0._dp
    real(dp) :: vern_temp_max = 0._dp
    real(dp) :: vern_fact_min = 0._dp
    integer :: vern_days_start = 0
    integer :: vern_days_end = 0
    real(dp) :: vern_curve_slope = 0._dp

    ! Photoperiod response: 0 = day-neutral, 1 = long-day, 2 = short-day
    integer :: photoperiod_response = 0
    real(dp) :: daylength_if = 0._dp
    real(dp) :: daylength_ins = 0._dp

    ! Yield and water-stress parameters
    real(dp) :: water_productivity = 0._dp
    real(dp) :: sink_strength = 0._dp
    real(dp) :: heat_stress_temp_crit = 0._dp
    real(dp) :: heat_stress_temp_lim = 0._dp
    real(dp) :: harvest_index = 0._dp
    real(dp) :: yield_response_total = 0._dp
    real(dp), dimension(4) :: yield_response_stage = 0._dp

    real(dp) :: raw_fraction = 0._dp
    real(dp) :: maximum_transpirative_root_fraction = 1._dp
    real(dp) :: interception_coef = 0._dp
    integer :: cn_class = 0
    logical :: is_irrigated = .false.

    ! Kcb correction parameters
    logical :: adjust_k_cb = .true.
    integer :: kcb_corr_start_day = 0
    integer :: kcb_corr_start_month = 0
    integer :: kcb_corr_end_day = 0
    integer :: kcb_corr_end_month = 0

    ! Crop development curves indexed by accumulated growing degree days.
    real(dp), dimension(:), allocatable :: gdd
    real(dp), dimension(:), allocatable :: k_cb
    real(dp), dimension(:), allocatable :: lai
    real(dp), dimension(:), allocatable :: height
    real(dp), dimension(:), allocatable :: root_depth
    real(dp), dimension(:), allocatable :: yield_response
    real(dp), dimension(:), allocatable :: cn_value
    real(dp), dimension(:), allocatable :: cover_fraction
    real(dp), dimension(:), allocatable :: stress_resistance
end type crop_definition

type crop_rotation
    ! Ordered crop-definition IDs associated with one land-use class.
    integer :: land_use_id = 0
    integer, dimension(:), allocatable :: crop_ids
    logical :: missed_sowing_warned = .false.
end type crop_rotation
! ---------------------------------------------------------------------------------

type crop_pars_matrices
    ! store the crop parameters for each calculation cell

    integer, dimension(:,:), allocatable :: crop_id
    integer, dimension(:,:), allocatable :: sowing_year
    integer, dimension(:,:), allocatable :: sowing_doy          ! Sowing DOY for the current crop in this cell, affected by irandom and sowing requirements (minimum temperature, cropsoverlap)
    integer, dimension(:,:), allocatable :: cuts_completed
    integer, dimension(:,:), allocatable :: rotation_position   ! Position of the crop in the rotation (1 = first crop, 2 = second crop, etc.)
    integer, dimension(:,:), allocatable :: rotation_land_use   ! Land use ID to which rotation_position belongs (can differ from the current ID if a crop survived a land-use change)
    integer, dimension(:,:), allocatable :: bare_soil_days_left
    logical, dimension(:,:), allocatable :: harvest_pending
    logical, dimension(:,:), allocatable :: is_real_crop        ! Used to distinguish between real crops and bare soil variations. Current discrimination: maxgdd>0
    real(dp), dimension(:,:), allocatable :: gdd                ! Accumulated growing degree days
    real(dp), dimension(:,:), allocatable :: vern_days          ! Accumulated vernalization days
    integer, dimension(:,:), allocatable :: crop_age_days       ! Days since sowing (sowing day is day 1)
    integer, dimension(:,:,:), allocatable :: curve_day         ! Crop age [days] at which each GDD threshold is reached. Used to construct phenology curves

    real(dp), dimension(:,:), allocatable :: k_cb
    real(dp), dimension(:,:), allocatable :: h
    real(dp), dimension(:,:), allocatable :: d_r
    real(dp), dimension(:,:), allocatable :: lai
    integer, dimension(:,:), allocatable :: cn_day
    real(dp), dimension(:,:), allocatable :: f_c
    integer, dimension(:,:), allocatable :: irrigation_class
    integer, dimension(:,:), allocatable :: cn_class
    real(dp), dimension(:,:), allocatable :: p
    real(dp), dimension(:,:), allocatable :: a
    real(dp), dimension(:,:), allocatable :: d_t_max
    real(dp), dimension(:,:), allocatable :: RF_t_max
    real(dp), dimension(:,:), allocatable :: RF_e
    real(dp), dimension(:,:), allocatable :: RF_t
    real(dp), dimension(:,:), allocatable :: p_day
    real(dp), dimension(:,:), allocatable :: k_cb_old           ! Previous day's Kcb value
    integer, dimension(:,:), allocatable :: pheno_stage         ! 0 = pre-emergence, 1 = stage, 2 = development, 3 = mid-season, 4 = late season. See FAO56
    real(dp), dimension(:,:), allocatable :: r_stress           ! Plant resistance to (water) stress
end type crop_pars_matrices

contains

subroutine calculate_RF_t(d_t, crop_par_mat,domain)
    ! calculate root fraction in both evaporative and transpirative layer
    real(dp), dimension(:,:), intent(in)::d_t
    type(grid_i),intent(in)::domain
    type(crop_pars_matrices),intent(inout)::crop_par_mat

    ! populate pheno%RF_t & pheno%RF_e
    where (domain%mat /= domain%header%nan .and. &
           crop_par_mat%d_t_max > 0.0D0 .and. &
           crop_par_mat%RF_t_max >= 0.0D0 .and. crop_par_mat%RF_t_max <= 1.0D0)
        crop_par_mat%RF_t = d_t/crop_par_mat%d_t_max
        where (crop_par_mat%RF_t_max*crop_par_mat%RF_t + &
               (1.0D0-crop_par_mat%RF_t_max) > tiny(1.0D0))
            crop_par_mat%RF_t = crop_par_mat%RF_t_max*crop_par_mat%RF_t / &
                (crop_par_mat%RF_t_max*crop_par_mat%RF_t + (1.0D0-crop_par_mat%RF_t_max))
        elsewhere
            crop_par_mat%RF_t = 0.0D0
        end where
    elsewhere
        crop_par_mat%RF_t = 0.0D0
    end where

    crop_par_mat%RF_t = round_2darray(crop_par_mat%RF_t,6)
    crop_par_mat%RF_e = 1.0D0-crop_par_mat%RF_t

    where (domain%mat == domain%header%nan)
        crop_par_mat%RF_t = dble(domain%header%nan)
        crop_par_mat%RF_e = dble(domain%header%nan)
    end where

end subroutine calculate_RF_t


end module mod_crop_phenology
