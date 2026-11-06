module mod_crop_yield
use mod_constants, only: dp
use mod_grid, only: grid_i
use mod_common, only: balance_matrices
use mod_meteo, only: meteo_mat
use mod_crop_phenology, only: crop_pars_matrices, crop_definition
use mod_cropcoef, only: adjust_water_productivity
implicit none

type yield_3d_t
    character(len=255) :: fn = ''
    real(dp), dimension(:,:,:), allocatable :: mat !(nrows,ncols,n_crops_in_year)
end type yield_3d_t

type yield_4d_t
    character(len=255) :: fn = ''
    real(dp), dimension(:,:,:,:), allocatable :: mat !(nrows,ncols,n_stages,n_crops_in_year)
end type yield_4d_t

type yield_t
    type(yield_3d_t) :: biomass_pot     ! Potential biomass [t/ha]
    type(yield_3d_t) :: yield_pot       ! Potential yield [t/ha]
    type(yield_3d_t) :: yield_act       ! Actual yield [t/ha]
    type(yield_3d_t) :: T_pot_ratio_sum ! Daily transpiration ratio (t_pot/et0) accumulator; used to estimate biomass_pot from water productivity.
    type(yield_3d_t) :: HS_sum          ! Daily Heat stress accumulator. Cool days add 1, stress days add <1. [-]
    type(yield_3d_t) :: f_HS            ! Mean heat-stress yield factor over the sensitive window, calculated as HS_sum/window_days [-]
    type(yield_3d_t) :: f_WS_tot        ! Water stress yield factor calculated from the whole crop cycle [-]
    type(yield_3d_t) :: f_WS_stage      ! Water stress yield factor calculated stage-by-stage [-]
    type(yield_4d_t) :: T_act_sum       ! Cumulative transpiration in each stage [mm]
    type(yield_4d_t) :: T_pot_sum       ! Cumulative potential transpiration in each stage [mm]
    type(yield_4d_t) :: days_in_stage   ! Number of days spent in each stage !TODO: should be an integer
    integer, allocatable :: completed_crops(:,:) ! Number harvested in each cell this calendar year
end type yield_t

! One growing crop per cell. These sums survive December 31 and the warmup handoff.
type yield_accumulator
    integer, allocatable :: crop_id(:,:)
    integer, allocatable :: sowing_year(:,:), sowing_doy(:,:)
    real(dp), allocatable :: transp_ratio(:,:), heat_sum(:,:)
    integer, allocatable :: heat_days(:,:)
    real(dp), allocatable :: transp_act(:,:,:), transp_pot(:,:,:), stage_days(:,:,:)
end type yield_accumulator

integer, parameter :: n_stages = 4

contains

! The accumulator follows each crop occurrence and is retained when crossing years and warmup handoffs
subroutine initialize_yield_accumulator(acc, domain)
    type(yield_accumulator), intent(inout) :: acc
    integer, intent(in) :: domain(:,:)
    integer :: rows, cols

    if (allocated(acc%crop_id)) return
    rows = size(domain, 1)
    cols = size(domain, 2)
    allocate(acc%crop_id(rows,cols), source=0)
    allocate(acc%sowing_year(rows,cols), acc%sowing_doy(rows,cols), source=0)
    allocate(acc%transp_ratio(rows,cols), acc%heat_sum(rows,cols))
    allocate(acc%heat_days(rows,cols))
    allocate(acc%transp_act(rows,cols,n_stages), acc%transp_pot(rows,cols,n_stages), acc%stage_days(rows,cols,n_stages))
    acc%transp_ratio = 0._dp
    acc%heat_sum = 0._dp
    acc%heat_days = 0
    acc%transp_act = 0._dp
    acc%transp_pot = 0._dp
    acc%stage_days = 0._dp
end subroutine initialize_yield_accumulator

subroutine start_yield_for_new_crops(acc, pheno, domain)
    type(yield_accumulator), intent(inout) :: acc
    type(crop_pars_matrices), intent(in) :: pheno
    type(grid_i), intent(in) :: domain
    integer :: i, j, crop_id

    do j = 1, size(domain%mat, 2)
        do i = 1, size(domain%mat, 1)
            if (domain%mat(i,j) == domain%header%nan) cycle
            crop_id = pheno%crop_id(i,j)
            if (crop_id == acc%crop_id(i,j) .and. &
                & pheno%sowing_year(i,j) == acc%sowing_year(i,j) .and. &
                & pheno%sowing_doy(i,j) == acc%sowing_doy(i,j)) cycle

            ! A changed land-use map can remove a crop without harvest.
            ! Its unfinished sums are discarded, not written as a yield.
            acc%crop_id(i,j) = 0
            acc%transp_ratio(i,j) = 0._dp
            acc%heat_sum(i,j) = 0._dp
            acc%heat_days(i,j) = 0
            acc%transp_act(i,j,:) = 0._dp
            acc%transp_pot(i,j,:) = 0._dp
            acc%stage_days(i,j,:) = 0._dp
            if (crop_id <= 0) cycle

            acc%crop_id(i,j) = crop_id
            acc%sowing_year(i,j) = pheno%sowing_year(i,j)
            acc%sowing_doy(i,j) = pheno%sowing_doy(i,j)
        end do
    end do
end subroutine start_yield_for_new_crops

subroutine initialize_yield(yld, domain, n_crops)
    type(yield_t), intent(inout) :: yld
    integer, dimension(:,:), intent(in) :: domain
    integer, intent(in) :: n_crops
    integer :: nrows, ncols

    nrows = size(domain, 1)
    ncols = size(domain, 2)
    allocate(yld%biomass_pot%mat    (nrows,ncols,n_crops), source=0.0_dp)
    allocate(yld%yield_pot%mat      (nrows,ncols,n_crops), source=0.0_dp)
    allocate(yld%yield_act%mat      (nrows,ncols,n_crops), source=0.0_dp)
    allocate(yld%f_WS_tot%mat       (nrows,ncols,n_crops), source=0.0_dp)
    allocate(yld%f_WS_stage%mat     (nrows,ncols,n_crops), source=0.0_dp)
    allocate(yld%f_HS%mat           (nrows,ncols,n_crops), source=0.0_dp)
    allocate(yld%HS_sum%mat         (nrows,ncols,n_crops), source=0.0_dp)
    allocate(yld%T_pot_ratio_sum%mat(nrows,ncols,n_crops), source=0.0_dp)
    allocate(yld%T_act_sum%mat      (nrows,ncols,n_stages,n_crops), source=0.0_dp)
    allocate(yld%T_pot_sum%mat      (nrows,ncols,n_stages,n_crops), source=0.0_dp)
    allocate(yld%days_in_stage%mat  (nrows,ncols,n_stages,n_crops), source=0.0_dp)
    allocate(yld%completed_crops    (nrows,ncols), source=0)
end subroutine initialize_yield

subroutine destroy_yield(yld)
    type(yield_t), intent(inout) :: yld

    if (allocated(yld%biomass_pot%mat))     deallocate(yld%biomass_pot%mat)
    if (allocated(yld%yield_pot%mat))       deallocate(yld%yield_pot%mat)
    if (allocated(yld%yield_act%mat))       deallocate(yld%yield_act%mat)
    if (allocated(yld%f_WS_tot%mat))        deallocate(yld%f_WS_tot%mat)
    if (allocated(yld%f_WS_stage%mat))      deallocate(yld%f_WS_stage%mat)
    if (allocated(yld%f_HS%mat))            deallocate(yld%f_HS%mat)
    if (allocated(yld%HS_sum%mat))          deallocate(yld%HS_sum%mat)
    if (allocated(yld%T_pot_ratio_sum%mat)) deallocate(yld%T_pot_ratio_sum%mat)
    if (allocated(yld%T_act_sum%mat))       deallocate(yld%T_act_sum%mat)
    if (allocated(yld%T_pot_sum%mat))       deallocate(yld%T_pot_sum%mat)
    if (allocated(yld%days_in_stage%mat))   deallocate(yld%days_in_stage%mat)
    if (allocated(yld%completed_crops))     deallocate(yld%completed_crops)
end subroutine destroy_yield

subroutine accumulate_daily_yield(acc, pheno, crops, meteo, balance, domain)
    type(yield_accumulator), intent(inout) :: acc
    type(crop_pars_matrices), intent(in) :: pheno
    type(crop_definition), intent(in) :: crops(:)
    type(meteo_mat), intent(in) :: meteo
    type(balance_matrices), intent(in) :: balance
    type(grid_i), intent(in) :: domain
    integer :: i, j, stage, crop_id
    real(dp) :: h_transp_act, h_transp_pot, progress, target_gdd, temp_crit, temp_lim

    do j = 1, size(domain%mat, 2)
        do i = 1, size(domain%mat, 1)
            if (domain%mat(i,j) /= domain%header%nan) then

                crop_id = acc%crop_id(i,j)
                if (crop_id == 0 .or. crop_id /= pheno%crop_id(i,j)) cycle

                stage = pheno%pheno_stage(i,j)
                h_transp_act = balance%lyr(1)%h_transp_act(i,j) + balance%lyr(2)%h_transp_act(i,j)
                h_transp_pot = balance%lyr(1)%h_transp_pot(i,j) + balance%lyr(2)%h_transp_pot(i,j)

                ! Accumulate thermal stress
                target_gdd = real(crops(crop_id)%max_cuts, dp) * maxval(crops(crop_id)%gdd)
                progress = pheno%gdd(i,j) / max(target_gdd, tiny(1._dp))
                if (progress >= 0.45_dp .and. progress < 0.75_dp) then
                    temp_crit = crops(crop_id)%heat_stress_temp_crit
                    temp_lim = crops(crop_id)%heat_stress_temp_lim
                    acc%heat_days(i,j) = acc%heat_days(i,j) + 1
                    if (meteo%T_ave(i,j) < temp_crit) then
                        acc%heat_sum(i,j) = acc%heat_sum(i,j) + 1._dp
                    else if (meteo%T_ave(i,j) < temp_lim .and. temp_lim > temp_crit) then
                        acc%heat_sum(i,j) = acc%heat_sum(i,j) + 1._dp - (meteo%T_ave(i,j) - temp_crit)/(temp_lim - temp_crit)
                    end if
                end if

                ! Accumulate water stress
                if (stage > 0) then
                    acc%transp_act(i,j,stage) = acc%transp_act(i,j,stage) + h_transp_act
                    acc%transp_pot(i,j,stage) = acc%transp_pot(i,j,stage) + h_transp_pot
                    acc%stage_days(i,j,stage) = acc%stage_days(i,j,stage) + 1._dp
                end if
                ! Update transpiration ratio
                if (meteo%et0(i,j) > 0._dp) acc%transp_ratio(i,j) = acc%transp_ratio(i,j) + h_transp_pot/meteo%et0(i,j)
            end if
        end do
    end do
end subroutine accumulate_daily_yield

subroutine finalize_harvested_yield(acc, yld, harvest_pending, crops, co2, domain)
    type(yield_accumulator), intent(inout) :: acc
    type(yield_t), intent(inout) :: yld
    logical, intent(in) :: harvest_pending(:,:)
    type(crop_definition), intent(in) :: crops(:)
    real(dp), intent(in) :: co2
    type(grid_i), intent(in) :: domain
    integer :: i, j, slot, stage, crop_id
    real(dp) :: total_pot, total_days, deficit, duration_fraction, wp_adj

    do j = 1, size(domain%mat, 2)
        do i = 1, size(domain%mat, 1)

            ! Only consider active cells whose crop is ready for harvest
            if (domain%mat(i,j) == domain%header%nan) cycle
            if (.not. harvest_pending(i,j)) cycle

            crop_id = acc%crop_id(i,j)
            slot = yld%completed_crops(i,j) + 1
            if (slot > size(yld%yield_act%mat, 3)) then
                print *, 'Too many harvested crops in one cell and calendar year. Execution will be aborted...'
                stop
            end if
            yld%completed_crops(i,j) = slot
            yld%T_pot_ratio_sum%mat(i,j,slot) = acc%transp_ratio(i,j)
            yld%HS_sum%mat(i,j,slot) = acc%heat_sum(i,j)
            yld%T_act_sum%mat(i,j,:,slot) = acc%transp_act(i,j,:)
            yld%T_pot_sum%mat(i,j,:,slot) = acc%transp_pot(i,j,:)
            yld%days_in_stage%mat(i,j,:,slot) = acc%stage_days(i,j,:)
            ! Calculate potential yield
            wp_adj = adjust_water_productivity(crops(crop_id)%water_productivity, co2, crops(crop_id)%sink_strength)
            yld%biomass_pot%mat(i,j,slot) = wp_adj * acc%transp_ratio(i,j)
            yld%yield_pot%mat(i,j,slot) = yld%biomass_pot%mat(i,j,slot) * crops(crop_id)%harvest_index

            yld%f_HS%mat(i,j,slot) = 1._dp
            if (acc%heat_days(i,j) > 0) yld%f_HS%mat(i,j,slot) = acc%heat_sum(i,j)/real(acc%heat_days(i,j), dp)
            ! "Total" water stress
            total_pot = sum(acc%transp_pot(i,j,:))
            yld%f_WS_tot%mat(i,j,slot) = 1._dp
            if (total_pot > 0._dp) then
                yld%f_WS_tot%mat(i,j,slot) = max(0._dp, &
                    & 1._dp - crops(crop_id)%yield_response_total*(1._dp - sum(acc%transp_act(i,j,:))/total_pot))
            end if

            ! Stage-specific water stress
            total_days = sum(acc%stage_days(i,j,:)) !%PS%: note that total_days does not include days in stage 0
            yld%f_WS_stage%mat(i,j,slot) = 1._dp
            if (total_days > 0._dp) then
                do stage = 1, n_stages
                    if (acc%stage_days(i,j,stage) == 0._dp .or. acc%transp_pot(i,j,stage) <= 0._dp) cycle
                    deficit = 1._dp - acc%transp_act(i,j,stage)/acc%transp_pot(i,j,stage)
                    duration_fraction = acc%stage_days(i,j,stage)/total_days
                    yld%f_WS_stage%mat(i,j,slot) = yld%f_WS_stage%mat(i,j,slot) * &
                        & max(0._dp, 1._dp - crops(crop_id)%yield_response_stage(stage)*deficit)**duration_fraction
                end do
            end if
            ! Actual yield depends on both heat and water stress
            yld%yield_act%mat(i,j,slot) = yld%yield_pot%mat(i,j,slot) * &
                & min(yld%f_WS_tot%mat(i,j,slot), yld%f_WS_stage%mat(i,j,slot)) * yld%f_HS%mat(i,j,slot)
            acc%crop_id(i,j) = 0
        end do
    end do
end subroutine finalize_harvested_yield

end module mod_crop_yield
