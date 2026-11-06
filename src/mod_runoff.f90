module mod_runoff
use mod_constants, only: dp
use mod_grid, only: grid_i, grid_r
use mod_common, only: moisture
implicit none


type output_cn
    real(dp)::tab_cn2              ! CN tabulated values (CN2)
    real(dp)::tab_cn3              ! CN value for antecedent moisture condition III (CN3)
    real(dp)::tab_cn2_baresoil     ! CN tabulated values (CN2) for bare soil
    real(dp)::tab_cn2_slope        ! slope adjusted CN value
    real(dp)::cn1_day              ! (slope and phenological phase) adjusted CN value for antecedent moisture condition I (CN1)
    real(dp)::cn2_day              ! (slope and phenological phase) adjusted CN value for antecedent moisture condition II (CN2)
    real(dp)::cn3_day              ! (slope and phenological phase) adjusted CN value for antecedent moisture condition III (CN3)
    real(dp)::cn_day               ! output CN values - (soil moisture, slope and phenological phase) adjusted
    integer::k                     ! table CN index - cover type
    integer::jj                    ! table CN index - hydrological condition (1=good, 2=fair, 3=poor)
    integer::ii                    ! table CN index - hydrological class (1=A, 2=B, 3=C, 4=D)
end type output_cn


contains

subroutine CN_runoff(gross_av_water,net_av_water,h_irr, domain, is_rice_paddy, runoff, cn, lambda_cn)
    ! Runoff calculation according to SCS-CN method (daily calculation)
    real(dp),dimension(:,:),intent(in)::h_irr                      ! irrigation water [mm]
    type(grid_i),intent(in)::domain                                ! simulation domain [-,-]
    logical, dimension(:,:), intent(in) :: is_rice_paddy           ! Identifies rice paddies, which get runoff=0
    real(dp),dimension(:,:),intent(in)::cn                         ! cn distribution
    real(dp),intent(in)::lambda_cn                                 ! lambda CN [-]]
    real(dp),dimension(:,:),intent(inout)::gross_av_water          ! water available [mm]
    real(dp),dimension(:,:),intent(inout)::net_av_water            ! water available for infiltration [mm]
    real(dp),dimension(:,:),intent(out)::runoff                    ! runoff [mm]
    real(dp),dimension(size(domain%mat,1),size(domain%mat,2))::S   ! potential maximum retention after runoff begins [mm]
    real(dp),dimension(size(domain%mat,1),size(domain%mat,2))::Ia  ! initial abstractions [mm]

    S=0.;Ia=0.;runoff=0.

    where(domain%mat/=domain%header%nan)
        where (.not. is_rice_paddy) ! Rice receives runoff=0 (as initialized)
            S=25.4*((1000./cn)-10.)
            Ia=lambda_cn*S
            runoff = ((gross_av_water-Ia)**2.)/(gross_av_water+0.8*S)
            runoff = merge (runoff,0.0D0,gross_av_water > Ia)
            runoff = merge (runoff, net_av_water, net_av_water > runoff)
            net_av_water = net_av_water - runoff
        end where
    end where
    gross_av_water=h_irr+gross_av_water
end subroutine CN_runoff


subroutine CN_table(tab_CN2, tab_CN3,hydr_cond,cn,cn_day,cn_day_out,domain,hydr_grp, &
    & slope, cn_out,theta,t_soil1,t_soil2)
    ! CN model
   ! Input
    real(dp),dimension(:,:,:),intent(in)::tab_CN2, tab_CN3
    type(grid_i),intent(in)::domain                                    ! simulation domain
    type(grid_i),intent(in)::hydr_grp                                  ! hydrological group [-] [integer, 1-4 - corresponds to A-D classes]
    type(grid_i),intent(in)::hydr_cond                                 ! hydrological condition [-] [integer, 1-3]
    real(dp), dimension(:,:),intent(in)::slope                         ! slope [100*m/m]
    integer,dimension(:,:),intent(in)::cn
    integer,dimension(:,:),intent(in)::cn_day
    type(moisture),dimension(:),intent(in)::theta
    ! TODO: pass only the total soil moisture
    real(dp),dimension(:,:),intent(in)::t_soil1,t_soil2                 ! soil water content from balance [m3/m3]
    ! Output
    real(dp),dimension(:,:),intent(out)::cn_day_out
    type(output_cn),dimension(:,:),intent(out)::cn_out
    ! Internal
    real(dp),dimension(size(tab_cn2,1),size(tab_cn2,2),size(tab_cn2,3))::tab_cn2_slope
    real(dp),dimension(size(domain%mat,1),size(domain%mat,2))::cn2
    real(dp),dimension(size(domain%mat,1),size(domain%mat,2))::cn1_day
    real(dp),dimension(size(domain%mat,1),size(domain%mat,2))::cn2_day
    real(dp),dimension(size(domain%mat,1),size(domain%mat,2))::cn3_day
    integer::i,j,k,ii,jj
    real(dp),dimension(size(domain%mat,1),size(domain%mat,2))::t_wp            ! moisture content at wilting point
    real(dp),dimension(size(domain%mat,1),size(domain%mat,2))::t_fc            ! moisture content at field capacity
    real(dp),dimension(size(domain%mat,1),size(domain%mat,2))::t_sat           ! moisture content at saturation
    real(dp),dimension(size(domain%mat,1),size(domain%mat,2))::t_amc2          ! moisture content at AMCII
    real(dp),dimension(size(domain%mat,1),size(domain%mat,2))::t_act           ! actual moisture content
    ! Setting moisture contents
    t_wp=0.;t_fc=0.;t_sat=0.;t_amc2=0.;
    where(domain%mat/=domain%header%nan)
        t_wp=theta(1)%wp%mat+theta(2)%wp%mat
        t_fc=theta(1)%fc%mat+theta(2)%fc%mat
        t_sat=theta(1)%sat%mat+theta(2)%sat%mat
        t_amc2=t_wp+(2.0/3.0)*(t_fc-t_wp)
        t_act=t_soil1+t_soil2
    end where
    ! CN Table
    tab_cn2_slope=0.
    cn_day_out=0.
    cn2    =0.
    cn1_day =0.;     cn2_day =0.;     cn3_day =0.

    ! Get CN values
    do j=1,size(domain%mat,2)
        do i=1,size(domain%mat,1)
            if(domain%mat(i,j)/=domain%header%nan)then
                ! Cover type - k-index:
                ! 1 - Crop Residue Cover; 2 - Row crops; 3 - Small grain
                ! 4 - Close-seeded or broadcast legumes or rotation meadow; 5 - Meadow; 6 - Woods � grass combination (orchard or tree farm)
                ! 7 - Rice; 8 - Fallow - Bare soil; 9 - Woods
                k = cn(i,j)

                ! Hydrological condition - jj-index:
                ! 1 - good; 2 - fair; 3 - poor
                jj = hydr_cond%mat(i,j)

                ! Hydrological group - ii-index
                ii=hydr_grp%mat(i,j)

                ! Slope adjustment - not calculated for rice
                call adjust_cn_by_slope(tab_CN2,tab_CN3,slope(i,j),tab_cn2_slope)
                ! Crop Residue Cover (k==1), Meadow (k==5) and Fallow (k==8): hydrological condition is not taken into account (jj=1)
                ! Rice (k==7): CN method is not taken into account when on field
                if (k==1 .or. k==5 .or. k==8) then      ! Crop Residue Cover (k==1), Meadow (k==5) and Fallow (k==8): hydrological condition is always 1
                    cn2(i,j)=tab_cn2_slope(k,1,ii)
                else
                    cn2(i,j)=tab_cn2_slope(k,jj,ii)
                end if

                ! Phenological adjustment - not calculated for rice
                ! Rice (k==7): CN method is not taken into account when on field
                call adjust_cn_by_pheno(cn_day(i,j),tab_cn2_slope(1,1,ii),cn2(i,j),cn2_day(i,j),k)     ! %AB% tab_CN2_slope(1,1,ii)=Crop Residue Cover CN2

                ! Soil moisture content adjustment
                ! CN1 & CN3
                cn1_day(i,j)=cn2_day(i,j)-((20.*(100.-cn2_day(i,j)))/(100.-cn2_day(i,j)+exp(2.533-0.0636*(100.-cn2_day(i,j)))))
                ! %EAC% limit lower value
                cn1_day(i,j) = max(cn1_day(i,j), .4 * cn2_day(i,j))

                cn3_day(i,j)=cn2_day(i,j)*exp(0.00673*(100.-cn2_day(i,j)))
                ! %EAC% limit upper value
                cn3_day(i,j) = min(cn3_day(i,j), 100.0D0)

                ! Soil moisture content CN
                call adjust_cn_by_moisture(cn_day_out(i,j),cn1_day(i,j),cn2_day(i,j),cn3_day(i,j), &
                    & t_wp(i,j),t_fc(i,j),t_sat(i,j),t_amc2(i,j),t_act(i,j))
                ! Output
                cn_out(i,j)%tab_cn2=tab_cn2(k,jj,ii)
                cn_out(i,j)%tab_cn3=tab_cn3(k,jj,ii)
                cn_out(i,j)%tab_cn2_baresoil=tab_cn2_slope(1,1,ii)
                cn_out(i,j)%k=k
                cn_out(i,j)%jj=jj
                cn_out(i,j)%jj=ii
                cn_out(i,j)%tab_cn2_slope=cn2(i,j)
                cn_out(i,j)%cn1_day=cn1_day(i,j)
                cn_out(i,j)%cn2_day=cn2_day(i,j)
                cn_out(i,j)%cn3_day=cn3_day(i,j)
                cn_out(i,j)%cn_day=cn_day_out(i,j)

            end if
        end do
    end do
end subroutine CN_table

subroutine adjust_cn_by_slope(tab_CN2,tab_CN3,slope,tab_CN2_slope)
    !slope CN adjustment
   real(dp),intent(in)::slope
    real(dp),dimension(:,:,:),intent(in)::tab_CN3,tab_CN2
    real(dp),dimension(:,:,:),intent(inout)::tab_CN2_slope

    integer::i,j,k

    do k=1,size(tab_CN2,1)
        do j=1,size(tab_CN2,2)
            do i=1,size(tab_CN2,3)
                tab_CN2_slope(k,j,i)=((tab_CN3(k,j,i)-tab_CN2(k,j,i))/3.)*(1.-2.*exp(-13.86*slope))+tab_CN2(k,j,i)
            end do
        end do
    end do
end subroutine adjust_cn_by_slope

subroutine adjust_cn_by_pheno(cn_day,tab_CN2_slope,cn2,cn2day,k)
    !phenological phase CN adjustment
   integer,intent(in)::cn_day
    real(dp),intent(in)::tab_CN2_slope
    real(dp),intent(in)::cn2
    real(dp),intent(out)::cn2day
    integer,intent(in)::k

    select case (cn_day)
        case (0); cn2day=tab_CN2_slope!         ! soil cover = 0 (bare soil)
        case (1); cn2day=cn2                    ! soil cover = 1 (growing phase)
        case (2)                                ! soil cover = 2 (peak)
            if (k==5 .or. k==6 .or. k==9) then  ! Meadow (k==5), Orchard (k==6) and Wood (k==9): soil cover is not taken into account
                cn2day=cn2
            else
                cn2day=2.*cn2-tab_CN2_slope
            endif
        case default
            print *, "Error in phenological parameters (CN)"
            cn2day=tab_CN2_slope
    end select
end subroutine adjust_cn_by_pheno

subroutine adjust_cn_by_moisture(cn,cn1,cn2,cn3,swp,sfc,ssat,sfcwp,st)
    !moisture content CN adjustment
   real(dp),intent(inout)::cn
    real(dp),intent(in)::cn1,cn2,cn3
    real(dp),intent(in)::swp,sfcwp,sfc,ssat,st ! moisture content: WP, AWCII, FC, saturation, actual [mm]
    real(dp),parameter::cn4=95.d0

    if (st <= swp) then
        cn = cn1
    else if(st < sfcwp)then
        cn = ((cn2-cn1)/(sfcwp-swp))*(st-swp)+cn1
    else if(st < sfc)then
        cn = ((cn3-cn2)/(sfc-sfcwp))*(st-sfcwp)+cn2
    else if(st < ssat)then
        cn = ((cn4-cn3)/(ssat-sfc))*(st-sfc)+cn3
    else if(st >= ssat)then
        cn=cn4
    end if
end subroutine adjust_cn_by_moisture


end module