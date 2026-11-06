module mod_irrigation
use mod_constants, only: dp, seconds_per_day
use mod_utility, only: get_value_index
use mod_grid
use mod_common, only: balance_matrices, spatial_info
use mod_crop_phenology, only: crop_pars_matrices
use mod_parameters, only: parameters, scheduled_irrigation, irr_units_table, source_info, water_sources_table
implicit none


contains

subroutine calc_irrigation_losses(a_loss, b_loss, c_loss, wind_vel, temp_ave,losses)
    ! calculate the water losses during the irrigation event
    ! according to the formula
    ! losses = a + (b * wind_vel) + (c * temp_ave)
    ! where a, b and c are coefficient
    real(dp),dimension(:,:),intent(in)::a_loss,b_loss,c_loss
    real(dp),dimension(:,:),intent(in)::wind_vel
    real(dp),dimension(:,:),intent(in)::temp_ave
    real(dp),dimension(:,:),intent(inout)::losses

    losses = a_loss + b_loss * wind_vel + c_loss * temp_ave
    losses = merge(losses,0.0D0,losses>0.0D0)
end subroutine calc_irrigation_losses

subroutine calc_perc_booster_pars(info_spat, met, quantiles)
    ! calculate the parameter for the percolation booster
    type(spatial_info),intent(inout)::info_spat
    type(par_method),dimension(:),intent(in)::met
    real(dp),dimension(info_spat%domain%header%imax,info_spat%domain%header%jmax)::a_min,a_max
    real(dp),dimension(info_spat%domain%header%imax,info_spat%domain%header%jmax)::b_min,b_max
    real(dp),dimension(:,:),intent(in)::quantiles
    integer::i

    a_min=0.d0;a_max=0.d0
    b_min=0.d0;b_max=0.d0
    a_min=id_to_par(info_spat%irr_meth_id,met(:)%a_min)
    a_max=id_to_par(info_spat%irr_meth_id,met(:)%a_max)
    b_min=id_to_par(info_spat%irr_meth_id,met(:)%b_min)
    b_max=id_to_par(info_spat%irr_meth_id,met(:)%b_max)

    do i=1,2
        info_spat%a_perc(i) = info_spat%domain
        info_spat%a_perc(i)%mat = 0.d0
        info_spat%b_perc(i) = info_spat%domain
        info_spat%b_perc(i)%mat = 0.d0

        where(info_spat%k_sat(i)%mat<=quantiles(i,1))
            info_spat%a_perc(i)%mat=a_max
            info_spat%b_perc(i)%mat=b_max
        else where(info_spat%k_sat(i)%mat>=quantiles(i,2))
            info_spat%a_perc(i)%mat=a_min
            info_spat%b_perc(i)%mat=b_min
        else where
            info_spat%a_perc(i)%mat = &
                & info_spat%k_sat(i)%mat*((a_max-a_min)/(quantiles(i,1)-quantiles(i,2))) + &
                & a_min - quantiles(i,2) * (a_max-a_min)/(quantiles(i,1)-quantiles(i,2))
            info_spat%b_perc(i)%mat = &
                & info_spat%k_sat(i)%mat*((b_max-b_min)/(quantiles(i,1)-quantiles(i,2))) + &
                & b_min - quantiles(i,2) * (b_max-b_min)/(quantiles(i,1)-quantiles(i,2))
        end where

        call set_default_par(info_spat%a_perc(i),info_spat%domain,1.0D0)
        call set_default_par(info_spat%b_perc(i),info_spat%domain,1.0D0)
    end do

    ! a_perc and b_perc are NaN for not irrigated cells

end subroutine calc_perc_booster_pars

subroutine update_adj_perco_parameters(info_spat, matrice_irr, day_from_irr, adj_perc_par)
    ! calculate adjustment parameter of the percolation model (eq 1.44)
    ! from the last irrigation event
    type(spatial_info),intent(in)::info_spat
    real(dp),dimension(:,:),intent(in)::matrice_irr
    integer,dimension(:,:),intent(inout)::day_from_irr
    real(dp),dimension(:,:,:),intent(inout)::adj_perc_par

    where(matrice_irr==0)
        where(day_from_irr==-9999)
            day_from_irr=-9999
        else where
            day_from_irr = day_from_irr +1
        end where
    else where
        day_from_irr=0
    end where

    ! %EAC%: limit day_from_irr where day_from_irr= 20 to prevent calculation error, TODO: to be fixed with a better solution
    ! 20 should be safe for all OS and CPU
    where(day_from_irr>20)
        day_from_irr=-9999 ! reset after 20 days from irrigation
    end where

    ! espeperc --> H_(x,irr)=(1+a_x e^(-t⋅b_x ))H_x
    ! the following causes exception
    ! adj_perc_par(:,:,1)=merge(1.0D0,1+info_spat%a_perc(1)%mat*exp(-1.0*day_from_irr*info_spat%b_perc(1)%mat),day_from_irr==-9999)
    ! adj_perc_par(:,:,2)=merge(1.0D0,1+info_spat%a_perc(2)%mat*exp(-1.0*day_from_irr*info_spat%b_perc(2)%mat),day_from_irr==-9999)
    where(day_from_irr==-9999)
        adj_perc_par(:,:,1)=1.0D0
        adj_perc_par(:,:,2)=1.0D0
    else where
        adj_perc_par(:,:,1)=1+info_spat%a_perc(1)%mat*exp(-1.0*day_from_irr*info_spat%b_perc(1)%mat)
        adj_perc_par(:,:,2)=1+info_spat%a_perc(2)%mat*exp(-1.0*day_from_irr*info_spat%b_perc(2)%mat)
    end where

end subroutine

subroutine irrigation_need_fixed(info_spat, h_irr, balance, balance_old, pheno, eff_rain, xrice_ksat, is_rice_paddy, h_sat2)
    ! calculate irrigation needs at field capacity and fixed volume (defined by irrigation methods)
   real(dp),dimension(:,:,:),intent(out)::h_irr
    type(spatial_info),intent(in)::info_spat
    type(balance_matrices),intent(in)::balance,balance_old
    type(crop_pars_matrices),intent(in)::pheno
    real(dp),dimension(:,:),intent(in)::eff_rain
    real(dp),intent(in)::xrice_ksat     ! ksat of the transpirative layer for rice
    logical, dimension(:,:), intent(in) :: is_rice_paddy
    real(dp),dimension(:,:),intent(in)::h_sat2
    real(dp),dimension(size(info_spat%domain%mat,1),size(info_spat%domain%mat,2))::h_irr_temp
    integer::i,j
    ! init
    h_irr = 0.
    ! the irrigation event is estimated only during the irrigation season
    h_irr_temp = 0.

    where(pheno%irrigation_class == 1 .and. info_spat%irr_meth_id%mat>0)
        !!! %RR% %CG% %EAC% %AB% (feb-23) network efficiency no more considered in need mode [info_spat%eff_rete%mat]
        where(is_rice_paddy)
            ! %CG% add water to fill soil, ponding and ET
            h_irr_temp = ((info_spat%h_meth%mat-balance_old%h_pond)+ &
                            (info_spat%theta(1)%sat%mat*balance_old%lyr(1)%thickness*1000-balance_old%lyr(1)%h_soil)+ &
                            (h_sat2-balance_old%lyr(2)%h_soil) + &
                            (balance_old%h_eva + balance_old%lyr(2)%h_transp_pot))/ &
                            1.0 ! don't consider efficiency
        else where ((balance_old%lyr(1)%h_soil + balance_old%lyr(2)%h_soil) < &
                    balance%h_irr_thresh)
            h_irr_temp = info_spat%h_meth%mat
        end where
    end where

    call irrigate_rice(h_irr_temp, pheno, eff_rain, xrice_ksat, is_rice_paddy)

    ! split the irrigation matrix for each irrigation method
    ! n is the number of irrigation methods
    forall(i=1:size(info_spat%domain%mat,1),j=1:size(info_spat%domain%mat,2),&
        & info_spat%irr_meth_id%mat(i,j)>0) &
        & h_irr(i,j,info_spat%irr_meth_id%mat(i,j)) = h_irr_temp(i,j)

end subroutine irrigation_need_fixed

subroutine irrigation_need_fc(info_spat, h_irr, balance, balance_old, pheno, eff_rain, xrice_ksat, is_rice_paddy, fc_ratio)
    ! calculate irrigation needs at field capacity
   real(dp),dimension(:,:,:),intent(out)::h_irr
    type(spatial_info),intent(in)::info_spat
    type(balance_matrices),intent(in)::balance,balance_old
    type(crop_pars_matrices),intent(in)::pheno
    real(dp),dimension(:,:),intent(in)::eff_rain
    integer::i,j
    real(dp),intent(in)::xrice_ksat     ! ksat of the transpirative layer for rice
    logical, dimension(:,:), intent(in) :: is_rice_paddy
    real(dp),dimension(size(info_spat%domain%mat,1),size(info_spat%domain%mat,2))::h_irr_temp
    real(dp),intent(in)::fc_ratio ! fraction of FC to use as target
    logical, save :: rice_fc_warning_shown = .false.
    ! init
    h_irr = 0.
    h_irr_temp = 0.

    if (any(is_rice_paddy) .and. .not. rice_fc_warning_shown) then
        print *, 'Warning: Mode 2 rice irrigation behaviour is inconsistent with other modes and requires checking:'
        print *, 'it replaces et and percolation, but does not have a moisture or ponding target'
        rice_fc_warning_shown = .true.
    end if

    !!! %RR% %CG% %EAC% %AB% (feb-23) network efficiency no more considered in need mode [info_spat%eff_rete%mat]
    where (pheno%irrigation_class == 1 .and. info_spat%irr_meth_id%mat>0)
        where (is_rice_paddy)
            h_irr_temp = (balance_old%h_eva + balance_old%lyr(2)%h_transp_pot)/(info_spat%eff_met%mat)

            ! TODO: info_spat%h_meth%mat is not initialized in FC mode
            !h_irr_temp = ((info_spat%h_meth%mat-bil1_old%h_pond)+ & ! fill the ponding layer
            !             (info_spat%theta(1)%sat%mat*bil1_old%d_e*1000-bil1_old%h_soil)+ &
            !             (info_spat%theta(2)%sat%mat*bil2%d_t*1000-bil2_old%h_soil) + & ! fill the soil layer to saturation
            !             (bil1_old%h_eva + bil2_old%h_transp_pot))/ & ! fill ET
            !             (info_spat%eff_met%mat) ! consider also efficiency

!~             else where (bil2_old%tmm<bil2%RAWbig)       !all other crops
        else where ((balance_old%lyr(1)%h_soil + balance_old%lyr(2)%h_soil) < &
                    balance%h_irr_thresh)
            ! TODO: %AB% why only 2nd layer RAWbig?
            where(balance_old%lyr(1)%h_soil <= info_spat%theta(1)%FC%mat*balance_old%lyr(1)%thickness*1000)
                h_irr_temp = fc_ratio *((info_spat%theta(1)%FC%mat * balance_old%lyr(1)%thickness * 1000 - balance_old%lyr(1)%h_soil) + &
                    & (info_spat%theta(2)%FC%mat * balance%lyr(2)%thickness * 1000 - balance_old%lyr(2)%h_soil))/(info_spat%eff_met%mat)
            else where
                h_irr_temp = fc_ratio*(info_spat%theta(2)%FC%mat * balance%lyr(2)%thickness * 1000 - balance_old%lyr(2)%h_soil)/&
                    & (info_spat%eff_met%mat)
            end where
        end where
    end where

    where (h_irr_temp <0.0D0)
        h_irr_temp = 0.0D0
    end where

    call irrigate_rice(h_irr_temp, pheno, eff_rain, xrice_ksat, is_rice_paddy)

    ! split the irrigation matrix for each irrigation method
    ! n is the number of irrigation methods
    forall(i=1:size(info_spat%domain%mat,1),j=1:size(info_spat%domain%mat,2),&
        & info_spat%irr_meth_id%mat(i,j)>0) &
        & h_irr(i,j,info_spat%irr_meth_id%mat(i,j)) = h_irr_temp(i,j)

end subroutine irrigation_need_fc

subroutine irrigation_scheduled(info_spat, doy_cur, year_cur, sch_irr, pheno, h_irr, verbose, balance, balance_old,         &
                              & a_loss, b_loss, c_loss, wind_vel, temp_ave,losses,eff_rain,xrice_ksat, is_rice_paddy, h_sat2)
    ! spread irrigation height when scheduled
    ! TODO: need for testing
    ! TODO: include losses calculation inside

    type(spatial_info),intent(in)::info_spat
    integer,intent(in)::doy_cur ! check the day as day of the year
    integer,intent(in)::year_cur ! check the year
    type(scheduled_irrigation),dimension(:),intent(in)::sch_irr
    type(crop_pars_matrices),intent(in)::pheno
    real(dp),dimension(:,:,:),intent(out)::h_irr
    logical:: verbose
    type(balance_matrices),intent(inout)::balance, balance_old

    real(dp),dimension(:,:),intent(in)::a_loss,b_loss,c_loss
    real(dp),dimension(:,:),intent(in)::wind_vel
    real(dp),dimension(:,:),intent(in)::temp_ave
    real(dp),dimension(:,:),intent(inout)::losses
    real(dp),dimension(:,:),intent(in)::eff_rain
    real(dp),intent(in)::xrice_ksat     ! ksat of the transpirative layer for rice
    logical, dimension(:,:), intent(in) :: is_rice_paddy
    real(dp),dimension(:,:),intent(in)::h_sat2

    logical, dimension(size(info_spat%domain%mat,1),size(info_spat%domain%mat,2)) :: is_irrigable
    real(dp),dimension(size(info_spat%domain%mat,1),size(info_spat%domain%mat,2))::h_irr_temp

    integer::i,j

    !init
    h_irr = 0.
    h_irr_temp = 0.
    losses = 0.

    where(info_spat%domain%mat/=info_spat%domain%header%nan .and. pheno%irrigation_class==1 .and. info_spat%irr_meth_id%mat>0)
        is_irrigable = .true.
    elsewhere
        is_irrigable = .false.
    end where

    ! loop throw scheduled irrigation list and update 'irrigation'
    do i=1,size(sch_irr)
        if (sch_irr(i)%year == year_cur .and. sch_irr(i)%doy == doy_cur) then
            if (verbose .eqv. .true.) then
                print *,'current year = ',  year_cur, '; current day = ', doy_cur, &
                    & '; irr.unit.id= ', sch_irr(i)%irr_unit_id, '; water depth = ', sch_irr(i)%h_irr, &
                    & '; h_irr_max= ', maxval(h_irr), '; h_irr_temp_max= ', maxval(h_irr_temp)
            end if
            if ((sch_irr(i)%h_irr > 0 .or. sch_irr(i)%h_irr == -1 .or. sch_irr(i)%h_irr == -10) .and. &
                & .not. any(info_spat%irr_unit_id%mat == sch_irr(i)%irr_unit_id .and. is_irrigable)) then
                print *, 'Warning: scheduled irrigation for IU ', sch_irr(i)%irr_unit_id, &
                    & ' on day ', doy_cur, ' but no irrigable cells were found.'
            end if
            if (sch_irr(i)%h_irr > 0) then
                ! add water quantity  as scheduled
                where (info_spat%irr_unit_id%mat == sch_irr(i)%irr_unit_id .and. is_irrigable)
                    h_irr_temp = sch_irr(i)%h_irr ! %EAC%: like the old version
                end where

                call calc_irrigation_losses(a_loss, b_loss, c_loss, wind_vel, temp_ave,losses)

            else if (sch_irr(i)%h_irr == -1) then
                ! add water with fixed volume
                where (info_spat%irr_unit_id%mat == sch_irr(i)%irr_unit_id .and. is_irrigable)
                    where(is_rice_paddy)
                        h_irr_temp = ((info_spat%h_meth%mat-balance_old%h_pond)+ &
                                    (info_spat%theta(1)%sat%mat*balance_old%lyr(1)%thickness*1000-balance_old%lyr(1)%h_soil)+ &
                                    (h_sat2-balance_old%lyr(2)%h_soil) + &
                                    (balance_old%h_eva + balance_old%lyr(2)%h_transp_pot))/ &
                                    1.0 ! don't consider efficiency
                    else where ((balance_old%lyr(1)%h_soil + balance_old%lyr(2)%h_soil) < &
                                balance%h_irr_thresh)
                        ! TODO: %AB% why only 2nd layer RAWbig?
                        ! else where (bil2_old%tmm<bil2%RAWbig)
                        h_irr_temp = info_spat%h_meth%mat
                    end where
                end where

                call calc_irrigation_losses(a_loss, b_loss, c_loss, wind_vel, temp_ave,losses)

            else if (sch_irr(i)%h_irr == -10) then
                ! add water to field capacity
                where (info_spat%irr_unit_id%mat == sch_irr(i)%irr_unit_id .and. is_irrigable)
                    where (is_rice_paddy)
                        h_irr_temp = ((info_spat%h_meth%mat-balance_old%h_pond)+ &
                                    (info_spat%theta(1)%sat%mat*balance_old%lyr(1)%thickness*1000-balance_old%lyr(1)%h_soil)+ &
                                    (h_sat2-balance_old%lyr(2)%h_soil) + &
                                    (balance_old%h_eva + balance_old%lyr(2)%h_transp_pot))/ &
                                    (info_spat%eff_met%mat) ! consider also efficiency
                        ! else where (bil2_old%tmm<bil2%RAWbig)       !all other crops
                    else where ((balance_old%lyr(1)%h_soil + balance_old%lyr(2)%h_soil) < &
                                balance%h_irr_thresh)
                        ! TODO: %AB% why only 2nd layer RAWbig?
                        where(balance_old%lyr(1)%h_soil <= info_spat%theta(1)%FC%mat*balance_old%lyr(1)%thickness*1000)
                            h_irr_temp = ((info_spat%theta(1)%FC%mat*balance_old%lyr(1)%thickness*1000-balance_old%lyr(1)%h_soil)+ &
                                & (info_spat%theta(2)%FC%mat*balance%lyr(2)%thickness*1000-balance_old%lyr(2)%h_soil))/(info_spat%eff_met%mat)
                        else where
                            h_irr_temp = (info_spat%theta(2)%FC%mat*balance%lyr(2)%thickness*1000-balance_old%lyr(2)%h_soil)/&
                                & (info_spat%eff_met%mat)
                        end where
                    end where
                end where

                losses = 0.

                ! OLD: force soil water content to field capacity
                ! set the soil water content [mm]
                !bil1%tmm = info_spat%theta(1)%fc%mat*1000*ze_fix
                !bil2%tmm = info_spat%theta(2)%fc%mat*1000*bil2%zr
                ! set the soil moisture [m3/m3]
                !bil1%tvol = info_spat%theta(1)%fc%mat
                !bil2%tvol = info_spat%theta(2)%fc%mat
            ! else do nothing
            end if
        end if
    end do

    call irrigate_rice(h_irr_temp, pheno, eff_rain, xrice_ksat, is_rice_paddy)

    ! split the irrigation matrix for each irrigation method
    ! n is the number of irrigation methods
    forall(i=1:size(info_spat%domain%mat,1),j=1:size(info_spat%domain%mat,2),&
        & info_spat%irr_meth_id%mat(i,j)>0) &
        & h_irr(i,j,info_spat%irr_meth_id%mat(i,j)) = h_irr_temp(i,j)

end subroutine irrigation_scheduled

subroutine irrigate_rice(h_irr, pheno, eff_rain, k_sat, is_rice_paddy)
    ! calculate irrigation event for paddy fields
    real(dp),dimension(:,:),intent(inout)::h_irr    ! TODO: externally set to equal the potential crop ET,
                                                    !       the soil water content to saturation and the ponding height
    type(crop_pars_matrices),intent(in)::pheno
    real(dp),dimension(:,:),intent(in)::eff_rain    ! effective rain
    real(dp),intent(in)::k_sat
    logical, dimension(:,:), intent(in) :: is_rice_paddy
    real(dp)::h_irr_min

    ! init the minimum irrigation height (equal to infiltration)
    h_irr_min = 10.*24.*k_sat ! k_sat is in mm
    where(is_rice_paddy)
        h_irr = max(0.0D0,h_irr_min + h_irr - eff_rain)  ! %CG%: irrigation compensate ET and Percolation, minus effective rain
        !where(h_irr<=h_irr_min) h_irr = h_irr_min       ! irrigation height at least equal to the minimum irrigation height
        !where(eff_rain>=h_irr) h_irr = 0.               ! if effective precipitation > irrigation height -> zero irrigation height
        ! adjust start and end of the season
        where(pheno%k_cb < pheno%k_cb_old .or. pheno%pheno_stage == 4) h_irr = 0 ! %CG%: if last crop period don't irrigate
    end where
end subroutine irrigate_rice

subroutine irrigation_use(domain, irr_units_map, irr_class, method, irr_units, transp_pot, h_soil_old, &
                        & h_irr_thresh, h_raw, h_priv_thresh, h_irr, doy, priv_irr, coll_irr,          &
                        & f_shape_area, cell_area, h_met, irr_starts, irr_ends, cn_class               )
    ! calculate irrigation heights in "USE" mode
    type(grid_i),intent(in)::domain, irr_units_map, method
    integer,dimension(:,:),intent(in)::irr_class, cn_class
    type(irr_units_table),dimension(:),intent(inout)::irr_units
    real(dp),dimension(:,:),intent(in)::h_soil_old                 ! average soil water content of previous day [mm]
    real(dp),dimension(:,:),intent(in)::transp_pot                 ! potential transpiration of previous day [mm]
    real(dp),dimension(:,:),intent(in)::h_irr_thresh, h_raw, h_priv_thresh
    real(dp),dimension(:,:,:),intent(out)::h_irr
    integer,intent(in)::doy
    real(dp),dimension(:,:),intent(inout)::priv_irr,coll_irr
    real(dp),dimension(:,:),intent(in)::cell_area
    real(dp),dimension(:,:),intent(in)::h_met
    integer, dimension(:,:), intent(in) :: irr_starts, irr_ends
    logical,intent(in)::f_shape_area

    integer :: i, j, k, shift, p
    integer :: n_cells_tobe_irr ! number of cells that might receive irrigation today (in-season & with an irrigable crop growing)
    integer :: n_cells_req_irr  ! number of cells that require irrigation from monitored sources today (irrigable + soil dry enough)
    real(dp),dimension(domain%header%imax,domain%header%jmax)::v_irr_cell   ! irrigation volumes [m^3]
    logical,dimension(domain%header%imax,domain%header%jmax)::irr_mask      ! a mask to get all the irrigable cells
    integer,dimension(:,:),allocatable::i_mat,j_mat,id_cell                 ! for movement inside the matrix
    integer,dimension(size(irr_units))::ind
    integer, dimension(:), allocatable :: vi, vj, vid, s_cn_class
    logical, dimension(:), allocatable :: reached_by_collective ! Marks the cells processed by collective sources (will be skipped by private sources)
    ! shifted copies of the already defined variable (see above)
    real(dp), dimension(:), allocatable :: s_h_old, s_irr_thresh, s_h_raw, s_priv_thresh, s_h_transp_pot
    real(dp), dimension(:), allocatable :: s_transp_ratio, s_def_day, s_v_irr_cell, s_h_met
    real(dp) :: Q_tot_act, Q_tot_pot, q_cell, q_act_avail, q_pot_avail, q_deliv, h_deliv
    integer :: n_cells_reached, n_cells_skip
    logical::rice_req,cell_req

    ! Flooded rice uses a ponding/saturation demand depth, but remains limited by source availability.

    if (f_shape_area .eqv. .false.) then
        v_irr_cell=1.e-3*h_met*(domain%header%cellsize**2) ! TODO: also NAN are calculated
    else
        v_irr_cell=1.e-3*h_met*cell_area
    end if

    h_irr = 0.

    ! init the matrix for coordinates and id of the calculation cells
    ind=0
    if(.not.(allocated(i_mat))) allocate(i_mat(domain%header%imax,domain%header%jmax))
    if(.not.(allocated(j_mat))) allocate(j_mat(domain%header%imax,domain%header%jmax))
    if(.not.(allocated(id_cell))) allocate(id_cell(domain%header%imax,domain%header%jmax))
    i_mat(:,1)=[(i,i=1,domain%header%imax)]
    i_mat=spread(i_mat(:,1),dim=2,ncopies=domain%header%jmax)
    j_mat(1,:)=[(j,j=1,domain%header%jmax)]
    j_mat=spread(j_mat(1,:),dim=1,ncopies=domain%header%imax)

    id_cell=0

    do j=1,domain%header%jmax
        do i=1,domain%header%imax
            do k=1,size(irr_units)
                if(irr_units_map%mat(i,j)==irr_units(k)%id .and. domain%mat(i,j)/=domain%header%nan)then
                    ind(k)=ind(k)+1
                    id_cell(i,j)=ind(k)
                end if
            end do
        end do
    end do

    irr_units_loop: do k=1,size(irr_units)
        ! select the cells that belong to the k-irr_units and are irrigable
        ! the mask includes also the rice cells
        !irr_mask=(irr_units_map%mat==irr_units(k)%id .and. irr_class==1) ! OLD CONTROL
        ! %EAC% add control for the local irrigation season
        Q_tot_act=0. ! total discharge actually delivered for irrigation
        Q_tot_pot=0. ! total discharge potentially delivered for irrigation

        irr_mask=(irr_units_map%mat==irr_units(k)%id &
                 .and. irr_class==1 &
                 .and. method%mat>0 &
                 .and. irr_starts<=doy &
                 .and. irr_ends>=doy)

        n_cells_tobe_irr = count(irr_mask)

        if (n_cells_tobe_irr == 0) then
            !%AB% skip the cycle if count(mask) == 0, no irrigable cells
            irr_units(k)%q_surplus = irr_units(k)%q_day
            irr_units(k)%q_day = 0
        else
            ! %AB% in case of irrigable cells
            ! %AB% vi(n) e vj(n) are the coordinates of the irrigable cells
            allocate(vi(n_cells_tobe_irr)); vi=pack(i_mat,irr_mask)
            allocate(vj(n_cells_tobe_irr)); vj=pack(j_mat,irr_mask)
            allocate(vid(n_cells_tobe_irr)); vid=pack(id_cell,irr_mask)
            ! the following are all the required parameters
            allocate(s_h_old(n_cells_tobe_irr)); s_h_old=pack(h_soil_old,irr_mask)
            allocate(s_irr_thresh(n_cells_tobe_irr)); s_irr_thresh=pack(h_irr_thresh,irr_mask)
            allocate(s_h_raw(n_cells_tobe_irr)); s_h_raw=pack(h_raw,irr_mask)
            allocate(s_priv_thresh(n_cells_tobe_irr)); s_priv_thresh=pack(h_priv_thresh,irr_mask)
            allocate(s_h_transp_pot(n_cells_tobe_irr)); s_h_transp_pot=pack(transp_pot,irr_mask)
            allocate(reached_by_collective(n_cells_tobe_irr)); reached_by_collective = .false.
            allocate(s_v_irr_cell(n_cells_tobe_irr)); s_v_irr_cell=pack(v_irr_cell,irr_mask)
            allocate(s_h_met(n_cells_tobe_irr)); s_h_met=pack(h_met,irr_mask)
            allocate(s_cn_class(n_cells_tobe_irr)); s_cn_class=pack(cn_class,irr_mask)
            n_cells_req_irr = count(s_h_old <= s_irr_thresh .or. (s_cn_class==7 .and. s_h_met>0.0D0))

            ! find the index of the latest irrigated cell (the day before)
            shift = 0
            if (any(vid == irr_units(k)%last_cell_id)) then
                shift = get_value_index(vid,irr_units(k)%last_cell_id)
            end if

            ! shift the vectors
            vi = cshift(vi,shift)
            vj = cshift(vj,shift)
            vid = cshift(vid,shift)
            s_h_old = cshift(s_h_old,shift)
            s_irr_thresh = cshift(s_irr_thresh,shift)
            s_h_raw = cshift(s_h_raw,shift)
            s_priv_thresh = cshift(s_priv_thresh,shift)
            s_h_transp_pot = cshift(s_h_transp_pot,shift)
            s_v_irr_cell = cshift(s_v_irr_cell,shift)
            s_h_met = cshift(s_h_met,shift)
            s_cn_class = cshift(s_cn_class,shift)

            ! %AB% check condition when the number of irrigable cells is very
            ! low respect the total number of cell in the irrigation unit

            cell_loop: do p=1, n_cells_tobe_irr !%AB% loop in irrigable cells where mask = 1

                ! discharge assigned to p-cell (not considering the field efficiency)
                q_cell = s_v_irr_cell(p) / seconds_per_day
                rice_req = (s_cn_class(p)==7 .and. s_h_met(p)>0.0D0)
                cell_req = (s_h_old(p)<=s_irr_thresh(p) .or. rice_req)
                q_act_avail = irr_units(k)%q_day - Q_tot_act
                q_pot_avail = irr_units(k)%q_day * irr_units(k)%f_explore - Q_tot_pot

                if(q_cell<=0.0D0) then
                    irr_units(k)%last_cell_id = vid(p)
                    reached_by_collective(p) = .true.
                    cycle cell_loop
                end if

                ! check if the water is enough for irrigation
                ! Flooded rice can use a partial application when today's source availability is
                ! lower than the full ponding/saturation request.
                if(rice_req .and. cell_req) then
                    if(q_act_avail<=0.0D0 .or. q_pot_avail<=0.0D0) exit cell_loop
                    q_deliv = min(q_cell,q_act_avail,q_pot_avail)
                else if((Q_tot_pot + q_cell) < (irr_units(k)%q_day * irr_units(k)%f_explore) .and. &
                    & (Q_tot_act + q_cell) <= irr_units(k)%q_day) then
                    q_deliv = q_cell
                else
                    exit cell_loop
                end if

                if(q_deliv<=0.0D0) exit cell_loop

                ! %AB% record the index of the current cell.
                ! It could be the latest irrigated in the current day and the first in the following
                Q_tot_pot = Q_tot_pot + q_deliv
                if(q_deliv>=q_cell) then
                    irr_units(k)%last_cell_id = vid(p)
                end if
                reached_by_collective(p) = .true.

                ! check if the cell is irrigable according to the soil water content less than the RAW big threshold
                ! %AB% alternative consider the number of days to have deficit
                if(cell_req) then !.or. dgg <= (n/IU(k)%n_irrigable_cells))then
                    ! %AB% apply the irrigation depth of the method
                    ! %AB% and sum the discharge actually delivered to the cell
                    h_deliv = s_h_met(p)
                    if(rice_req) h_deliv = s_h_met(p) * q_deliv/q_cell
                    coll_irr(vi(p),vj(p)) = h_deliv
                    Q_tot_act = Q_tot_act + q_deliv
                end if
            end do cell_loop

            ! %AB% update the irrigation period
            if (n_cells_tobe_irr /=0 .and. p<=n_cells_tobe_irr .and. Q_tot_act /= 0) then
                irr_units(k)%n_irrigated_cells = p
            else if (p>n_cells_tobe_irr) then ! all the cells are verified
                irr_units(k)%n_irrigated_cells = p-1
            else ! irrigation units with no irrigable cells
                irr_units(k)%n_irrigated_cells = 0
            end if

            ! mass conservation: how much water remains
            irr_units(k)%q_surplus = irr_units(k)%q_day - Q_tot_act

            ! Store the amount of water surplus for the next day if:
            ! 1 - the IU has at least a cell eligible for irrigation (irr_class == 1, in-season)
            ! 2 - at least one eligible cell requires collective-source irrigation !%PS%: Used to be "at least one cell was irrigated". Now surplus can be carried over to the next day if we are waiting for it to reach a "usable" threshold;
            ! 3 - not all cells in the IU were evaluated today                                                                        ! ; with this condition it is acceptable to simulate a IU receiving x m3/s every n days using a x/n m3/s daily flow,
            if (n_cells_tobe_irr/=0 .and. n_cells_req_irr /= 0 .and. irr_units(k)%n_irrigated_cells /= n_cells_tobe_irr) then         ! , even when the resulting flow is insufficient to trigger one irrigation event per day (as per original IdrAgra v1 behaviour)
                irr_units(k)%q_rem = irr_units(k)%q_surplus
                irr_units(k)%q_surplus = 0.  ! %AB% re-init the surplus
            else
                irr_units(k)%q_rem = 0.
            end if

            !%AB% store the water actually used
            irr_units(k)%q_day = Q_tot_act

            ! Unmonitored private sources (i.e. private wells)
            if (irr_units(k)%f_un_priv==1) then

                allocate(s_transp_ratio(n_cells_tobe_irr)); s_transp_ratio = 0.0_dp
                allocate(s_def_day(n_cells_tobe_irr)); s_def_day = 0.0_dp

                n_cells_reached = count(reached_by_collective) ! Cells already reached by collective sources

                ! Estimate the number of cells that will receive collective irrigation soon enough to avoid deficit
                n_cells_skip = 0
                if (sum(s_h_transp_pot) > 0.0_dp) then
                    s_transp_ratio = s_h_transp_pot / sum(s_h_transp_pot)
                    where(s_h_transp_pot /= 0.0_dp) s_def_day = (s_h_old - s_h_raw) / s_h_transp_pot ! Days until deficit
                    n_cells_skip =max(0,nint((sum(s_transp_ratio*s_def_day)/sum(s_transp_ratio))*irr_units(k)%n_irrigable_cells))
                end if

                ! Activate wells for cells that 1. are in need and 2. won't be reached by collective irrigation soon
                do p = 1, n_cells_tobe_irr
                    if ((s_h_old(p) <= s_priv_thresh(p)) .and. (p > n_cells_reached + n_cells_skip)) then
                        ! %AB% irrigation volume is equal to the irrigation depth of the method
                        priv_irr(vi(p),vj(p)) = s_h_met(p)
                        irr_units(k)%q_un_priv = irr_units(k)%q_un_priv + s_v_irr_cell(p)/seconds_per_day
                    end if
                end do

                deallocate(s_transp_ratio, s_def_day)
            end if

            deallocate(vi, vj, vid, s_cn_class, reached_by_collective, s_h_old, s_irr_thresh, s_h_raw, &
                     & s_priv_thresh, s_h_transp_pot, s_v_irr_cell, s_h_met                            )
        end if
    end do irr_units_loop ! end irrigation units loop

    ! check coll_irr(i,j)/=priv_irr(i,j) e record irrigated volumes
    ! TODO: split irrigation sources
    do j=1,domain%header%jmax
        do i=1,domain%header%imax
            if(domain%mat(i,j)/=domain%header%nan .and. method%mat(i,j)>0)then
                if(.not.(priv_irr(i,j)/=0. .and. coll_irr(i,j)/=0.))then
                    h_irr(i,j,method%mat(i,j)) = priv_irr(i,j)+coll_irr(i,j)
                else
                    print*,"Some cells are irrigated both from wells and diversions"
                end if
            end if
        end do
    end do

end subroutine irrigation_use

subroutine calc_daily_duty(cur_doy, irr_units, sources_info, wat_sources, irr_units_map, domain_map,    &
                         & par, irrigation_class, irrigation_method, h_soil_old, h_transp_pot, raw, h_fc)
    ! estimate the water volume for irrigation available in each irrigation units
    integer,intent(in)::cur_doy ! current day of simulation
    type(irr_units_table), dimension(:),intent(inout)::irr_units
    type(source_info),intent(in)::sources_info
    type(water_sources_table),dimension(:),intent(in)::wat_sources
    type(grid_i),intent(in)::irr_units_map,domain_map
    type(parameters),intent(in)::par
    integer,dimension(:,:),intent(in)::irrigation_class,irrigation_method
    real(dp),dimension(:,:),intent(in) :: h_soil_old, h_transp_pot, raw, h_fc

    integer,dimension(:,:),allocatable::cells_un_coll       ! map of the cells irrigated by unmonitored collective water sources
    real(dp)::frac_rel_un_coll                              ! fraction of water released respect to the maximum water available
    integer:: n_cells_irr_un_coll                           ! number of cells irrigated by unmonitored collective water sources
    integer:: n_cells_un_coll                               ! number of cells irrigable by unmonitored collective water sources
    integer::i,j,k

    ! estimate the daily duty for each irrigation units
    irr_units%q_act_fld(1)=0.;irr_units%q_act_fld(2)=0.;irr_units%q_act_fld(3)=0.;irr_units%q_act_fld(4)=0.;
    irr_units%q_day=0.
    irr_units%q_surplus=0.
    irr_units%n_irrigated_cells=0.
    irr_units%q_un_priv=0.
    irr_units%q_un_coll=0.
    frac_rel_un_coll = 0.

    if(.not.(allocated(cells_un_coll))) allocate(cells_un_coll(domain_map%header%imax, domain_map%header%jmax))
    cells_un_coll = 0
    if (par%cr%f_exists .eqv. .true.) then
        do i=1, size(wat_sources)
            ! %AB% irrigation_class is updated every days, so this cycle must be updated every day
            if (wat_sources(i)%type_id == 4) then
                where (irr_units_map%mat == wat_sources(i)%id_irr_unit &
                    & .and. domain_map%mat /= domain_map%header%nan &
                    & .and. irrigation_class == 1 &
                    & .and. irrigation_method > 0) &
                    & cells_un_coll = wat_sources(i)%wat_src_idx
            end if
        end do
    end if

    do i=1,size(wat_sources)
        j=wat_sources(i)%irr_unit_idx;
        if (j==0) then
            print*,'WARNING: water source ',wat_sources(i)%id_wat_src,' is not connected to any irrigation unit.'
            cycle
        end if
        k=wat_sources(i)%wat_src_idx
        select case(wat_sources(i)%type_id)
            case(1)
                irr_units(j)%q_act_fld(1) = wat_sources(i)%duty_frc * sources_info%mn_src_tbl1%q_daily(cur_doy,k) * irr_units(j)%int_distr_eff + irr_units(j)%q_act_fld(1)

            case(2)
                irr_units(j)%q_act_fld(2) = wat_sources(i)%duty_frc * sources_info%mn_src_tbl2%q_daily(cur_doy,k) * irr_units(j)%int_distr_eff + irr_units(j)%q_act_fld(2)

            case(3)
                irr_units(j)%q_act_fld(3) = wat_sources(i)%duty_frc * sources_info%int_reuse_tbl%q_daily(cur_doy,k) * irr_units(j)%int_distr_eff + irr_units(j)%q_act_fld(3)

            case(4) ! collective unmonitored sources
                frac_rel_un_coll = deliverable_ratio_unm_coll(cells_un_coll, h_soil_old, h_transp_pot,         &
                                                            & raw, h_fc, irrigation_class, par,                &
                                                            & domain_map%header%imax, domain_map%header%jmax, k)
                n_cells_irr_un_coll = count(cells_un_coll==k .and. irrigation_class==1)
                n_cells_un_coll = count(cells_un_coll==k)
                if(n_cells_un_coll==0) cycle
                irr_units(j)%q_act_fld(4)= &
                    & frac_rel_un_coll*wat_sources(i)%duty_frc * sources_info%unm_src_tbl%q_max(k) * irr_units(j)%int_distr_eff * (real(n_cells_irr_un_coll)/real(n_cells_un_coll)) + &
                    & irr_units(j)%q_act_fld(4)

                ! irr_units(j)%q_un_coll = &
                !     & frac_rel_un_coll*wat_sources(i)%duty_frc * sources_info%unm_src_tbl%q_max(k)*(real(n_cells_irr_un_coll)/real(n_cells_un_coll))+&
                !     & irr_units(j)%q_un_coll   ! only for printing %AB% gross used water

                ! at irrigation unit, alway consider the
                irr_units(j)%q_un_coll = irr_units(j)%q_act_fld(4)

            case default
        end select
    end do

    ! compute the water duty of each irrigation unit
    irr_units%q_day=irr_units%q_act_fld(1)+irr_units%q_act_fld(2)+irr_units%q_act_fld(3)+irr_units%q_act_fld(4)+irr_units%q_rem ! %AB% consider also the water surplus of the previous day
    ! reset q_rem to zero
    irr_units%q_rem = 0.0D0

end subroutine calc_daily_duty

function deliverable_ratio_unm_coll(cells_un_coll, h_soil, h_transp_pot, h_raw,   &
                                  & h_fc, irr_class, pars, imax, jmax, wat_src_idx)
    ! calculate the actual deliverable ratio from unmonitored water sources (e.g. collective wells plan)
    ! deliverable ratio is calculated from average water deficit of the served cells
    integer,intent(in)::imax,jmax
    integer,dimension(:,:),intent(in)::cells_un_coll     ! map of cells irrigated by unmonitored collective sources
    real(dp),dimension(:,:),intent(in) :: h_soil, h_transp_pot, h_raw, h_fc
    integer,dimension(:,:),intent(in)::irr_class
    type(parameters),intent(in)::pars
    integer,intent(in)::wat_src_idx

    real(dp)::transp_un_coll
    real(dp)::deficit_un_coll
    real(dp)::raw_un_coll
    real(dp),dimension(imax,jmax)::deficit_slp, raw_slp
    logical::deficit

    real(dp)::deliverable_ratio_unm_coll

    deliverable_ratio_unm_coll=0
    deficit_slp= (h_soil - h_raw) * h_transp_pot
    raw_slp = (h_fc - h_raw) * h_transp_pot

    transp_un_coll=sum(h_transp_pot, cells_un_coll == wat_src_idx .and. irr_class==1)

    if(transp_un_coll/=0.)then
        deficit_un_coll = sum(deficit_slp, cells_un_coll==wat_src_idx .and. irr_class==1)/transp_un_coll
        raw_un_coll=sum(raw_slp,cells_un_coll==wat_src_idx .and. irr_class==1)/transp_un_coll
    else
        deficit_un_coll=0.
        raw_un_coll=0.
    end if

    deficit = deficit_un_coll < (pars%uc_act_rules(wat_src_idx)%min_act_trsld*raw_un_coll)

    if(deficit .eqv. .true.)then
        if(deficit_un_coll <= pars%uc_act_rules(wat_src_idx)%flow_rate(3)*raw_un_coll) then
            deliverable_ratio_unm_coll = pars%uc_act_rules(wat_src_idx)%act_trsld(3)
            return
        else if(deficit_un_coll <= pars%uc_act_rules(wat_src_idx)%flow_rate(2)*raw_un_coll)then
            deliverable_ratio_unm_coll = pars%uc_act_rules(wat_src_idx)%act_trsld(2)
            return
        else if(deficit_un_coll <= pars%uc_act_rules(wat_src_idx)%flow_rate(1)*raw_un_coll)then
            deliverable_ratio_unm_coll = pars%uc_act_rules(wat_src_idx)%act_trsld(1)
            return
        end if
    end if

end function deliverable_ratio_unm_coll

end module
