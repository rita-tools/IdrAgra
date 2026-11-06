module mod_crop_soil_water
use mod_constants, only: dp
use mod_evapotranspiration
implicit none


contains

function cap_rise(sr, depth_under_rz, &
    & theta_act, theta_fc, theta_wp, &
    & transp_pot, eva_pot, &
    & a3, a4, b1, b2, b3, b4)
    ! Capillary rise model
    ! Reference:
    ! Liu, Pereira, Fernando, 2006
    ! Fluxes through the bottom boundary of the root zone in silty soils: Parametric approaches to estimate groundwater contribution and percolation
    ! Agricultural water management 84(2006):27-40

    real(dp),intent(in)::sr                 ! root zone depth (Sr) [m]
    real(dp),intent(in)::depth_under_rz      ! water table under root zone depth [m]

    ! Theta values in this function are dimensionless
    real(dp),intent(in)::theta_act           ! actual water content [-]
    real(dp),intent(in)::theta_fc            ! water content at field capacity [-]
    real(dp),intent(in)::theta_wp            ! water content at wilting point [-]

    real(dp),intent(in)::transp_pot          ! potential transpiration [mm/h]
    real(dp),intent(in)::eva_pot            ! potential evaporation [mm/h]

    real(dp),intent(in)::a3                 ! a3 value (Tables 5 & 7) [-]
    real(dp),intent(in)::a4                 ! a4 value (Tables 5 & 7) [-]
    real(dp),intent(in)::b1                 ! b1 value (Tables 5 & 7) [-]
    real(dp),intent(in)::b2                 ! b2 value (Tables 5 & 7) [-]
    real(dp),intent(in)::b3                 ! b3 value (Tables 5 & 7) [-]
    real(dp),intent(in)::b4                 ! b4 value (Tables 5 & 7) [-]

    real(dp)::cap_rise                      ! capillary rise (output) [mm/h]

    real(dp)::theta_wc                       ! critical soil water storage [-]
    real(dp)::theta_ss                       ! steady soil water storage [-]

    real(dp)::dwc                           ! critical groundwater depth [m]
    real(dp)::h_cap_pot                     ! potential capillary flux [mm/h]

    ! Soil water storage
    theta_wc = theta_fc*(1.+depth_under_rz)**b1                         ! Eq. 4
    theta_ss = 1.1*(theta_fc+theta_wp)/2. * (1+depth_under_rz)**b2      ! Eq. 5

    ! Critical groundwater depth (Eq. 6)
    if(transp_pot+eva_pot<=4./24.)then                               ! (if ET <= 4 mm d-1)
        dwc = a3*(transp_pot+eva_pot)*24.+b3+(-1.+sr) !EAC: adjust dwc if sr < 1
    else
        dwc =(a3*4.+b3) +(-1.+sr) ! EAC: in original paper is 1.4 but it is a general form
    end if

    ! Potential capillary flux (Eq. 7)
    h_cap_pot = (a4*(1+depth_under_rz)**b4)/24.
    if (transp_pot>0) then
        h_cap_pot = min(transp_pot, h_cap_pot) ! TODO: check --> apply only to crop
        ! if(1+depth_under_rz <= dwc)  gmax = max(transp_pot,gmax)
        if (1 + depth_under_rz <= dwc)  h_cap_pot = transp_pot
    end if

    cap_rise = h_cap_pot

    ! Capillary rise (Eq. 9)
    if(theta_act < theta_ss)then
        cap_rise = h_cap_pot
    else if(theta_act < theta_wc) then
        cap_rise = h_cap_pot*(theta_wc-theta_act)/(theta_wc-theta_ss)
    else
        cap_rise = 0.
    end if

end function cap_rise

function percolation_first_layer(theta_act,theta_r, theta_fc, theta_sat, k_sat, fatt_n)
    ! Percolation model suggested by %CG% Apr 2024

    real(dp),intent(in)::theta_act           ! actual water content [m3/m3 or mm]
    real(dp),intent(in)::theta_r             ! residual water content [m3/m3 or mm]
    real(dp),intent(in)::theta_fc            ! water content at field capacity [m3/m3 or mm]
    real(dp),intent(in)::theta_sat           ! saturated water content [m3/m3 or mm]

    !real(dp),intent(in)::et_first            ! evapotranspiration from the 1st layer

    real(dp),intent(in)::k_sat              ! saturated hydraulic conductivity [cm/h]
    real(dp),intent(in)::fatt_n             ! Brooks & Corey curve fitting parameter [-]
    real(dp)::k_uns                         ! hydraulic conductivity at unsaturated condition [cm/h]
    real(dp)::percolation_first_layer       ! percolation in selected time frame - (sub)hourly [mm/h]

    k_uns = k_sat*((theta_act-theta_r)/(theta_sat-theta_r))**fatt_n

    !print*, 'h_act:', theta_act,'h_fc:',theta_fc,'h_sat:',theta_sat !FAKE

    if(theta_act <= theta_r)then
        percolation_first_layer = 0.
    else if(theta_act > theta_sat)then
        percolation_first_layer=(theta_act-theta_fc) ! all the water above fc is percolation
    else if(theta_act > theta_fc)then
        percolation_first_layer=(theta_act-theta_fc) ! all the water above fc is percolation
    else
        percolation_first_layer = k_uns*10 ! theta_act < theta_fc
    end if

end function percolation_first_layer

function percolation(adj_perc_par,theta_act,theta_r,theta_sat,k_sat, fatt_n)
    ! Percolation model
    ! Reference:
    ! Brooks, Corey, 1966
    ! Properties of porous media affecting fluid flow
    ! J. Irr. Drain. Div. 92(1966):61-88

    real(dp),intent(in)::theta_act          ! actual water content [m3/m3 or mm]
    real(dp),intent(in)::theta_r            ! residual water content [m3/m3 or mm]
    real(dp),intent(in)::theta_sat          ! saturated water content [m3/m3 or mm]

    real(dp),intent(in)::k_sat              ! saturated hydraulic conductivity [cm/h]
    real(dp),intent(in)::fatt_n             ! Brooks & Corey curve fitting parameter [-]
    real(dp),intent(in)::adj_perc_par       ! factor that takes into account irrigation method and days spent from last irrigation [-]
    real(dp)::percolation                   ! percolation in selected time frame - (sub)hourly [mm/h]

    if(theta_act < theta_r)then
        percolation=0.
    else if(theta_act>= theta_sat)then
        percolation=k_sat*10                                                    ! cm/h -> mm/h
    else
    ! if ((teta_act >= teta_r_mm) .and. (teta_act < por_mm))
        percolation=k_sat*(((theta_act-theta_r)/(theta_sat-theta_r))**(fatt_n))*10  ! Brooks & Corey; cm/h -> mm/h
    end if

    ! Adjusted percolation
    percolation=percolation*adj_perc_par! TODO: %CG% not consider adj_perc_par
end function percolation

recursive subroutine water_balance_evap_lay(h_net_irr, h_soil1, &
    & h_inf, h_eva_act, h_eva_pot, h_perc1, h_pond0, h_pond, h_transp_act, h_transp_pot, k_cb, p_day, h_et0, &
    & k_e, k_r,k_stress_dry, k_stress_sat, hks, kc_max, few, rf_e, h_rew, h_sat, h_fc, h_wp, h_r, & !RR: add kr
    & k_sat, fatt_n, n_iter1, adj_perc_par, mmax, doy)
    ! water balance of the evapotranspirative layer
    ! recursive: time is divided by 2 in case solution not found

    real(dp), intent(in)::h_net_irr    ! net irrigation (not intercepted) [mm] - (sub)hourly
    real(dp), intent(in)::k_cb         ! base crop coefficient [-]
    real(dp), intent(in)::h_et0        ! reference evapotranspiration [mm]
    real(dp), intent(in)::h_inf        ! infiltration [mm] - (sub)hourly
    real(dp), intent(in)::kc_max       ! maximum value of Kc after irrigation or precipitation [-]
    real(dp), intent(in)::few          ! wetted soil surface ratio [-]
    real(dp), intent(in)::h_rew        ! residual water storage at REW [mm]
    real(dp), intent(in)::h_sat        ! water storage at saturation [mm]
    real(dp), intent(in)::h_fc         ! water storage at field capacity [mm]
    real(dp), intent(in)::h_wp         ! water storage at wilting point [mm]
    real(dp), intent(in)::h_r          ! residual water storage [mm]
    real(dp), intent(in)::k_sat        ! hydraulic conductivity at saturation [cm/h]
    real(dp), intent(in)::fatt_n       ! Brooks & Corey curve fitting parameter [-]
    real(dp), intent(in)::adj_perc_par ! factor that takes into account irrigation method and days spent from last irrigation [-]
    real(dp), intent(in)::p_day        ! deplection fraction adjusted for meteorological conditions [-]
    real(dp), intent(in)::rf_e         ! fraction of active roots in evaporative layer [mm]
    integer,intent(in)::doy            ! doy for debug

    real(dp), intent(inout)::h_soil1   ! soil water content of the layer [mm] - (sub)hourly
    real(dp), intent(inout)::h_pond0   ! ponding height at the beginning of the step [mm]
    integer, intent(inout)::mmax       ! maximum number of iteration [-]

    real(dp), intent(out)::k_e         ! actual soil evaporation coefficient [-]
    real(dp), intent(out)::k_r         ! reduction coefficient of evaporation [-]
    real(dp), intent(out):: k_stress_dry! water scarcity stress coefficient [-]
    real(dp), intent(out):: k_stress_sat! water saturation stress coefficient [-]
    real(dp), intent(out)::hks         ! water stress coefficient [-]
    real(dp), intent(out)::h_eva_act   ! effective evaporation [mm] - (sub)hourly
    real(dp), intent(out)::h_eva_pot   ! potential evaporation [mm] - (sub)hourly
    real(dp), intent(out)::h_transp_act! effective transpiration [mm] - (sub)hourly
    real(dp), intent(out)::h_transp_pot! potential transpiration [mm] - (sub)hourly
    real(dp), intent(out)::h_perc1     ! percolation from the first layer (mm) - (sub)hourly
    real(dp), intent(out)::h_pond      ! ponding height at the end of the step [mm]
    integer, intent(out)::n_iter1      ! final number of iteration [-]

    integer,parameter::n_max=20        ! maximum number of iterations
    integer, parameter::k_max = 2      ! time divider to reach convergence
    real(dp),parameter::converg=0.0001 ! convergence error [mm]

    real(dp)::h_soil0,h_soil_old,h_soil_mean,h_soil_new,h_delta
    integer::n,m,k
    real(dp)::h_eva_act_m, h_eva_pot_m, h_perc1_m, k_s_dry_m,k_s_sat_m, hks_m, h_transp_act_m, h_transp_pot_m
    real(dp)::h_pond_i                   ! ponding value to be used inside the cycle

    ! init
    h_soil0 = h_soil1                      ! teta1_old [mm]
    h_soil_old = h_soil0
    h_soil_mean = h_soil0
    h_pond_i = h_pond0

    do n=1,n_max
        ! TODO: check order and update h_soil_mean
        call evaporation(h_soil_mean, h_rew, h_wp, kc_max, few, k_e, k_cb,h_et0,h_eva_act,h_eva_pot,k_r)

        h_perc1 = percolation(adj_perc_par,h_soil_mean, h_r, h_sat, k_sat, fatt_n)
        !h_perc1 = percolation_first_layer(h_soil_mean, h_r, h_fc, h_sat, k_sat, fatt_n)

        call calculate_water_stresses(h_soil_mean,h_fc,h_wp,p_day,k_stress_dry,k_stress_sat)

        hks = min(k_stress_dry,k_stress_sat)

        call transpiration(k_cb,rf_e, h_transp_act, h_transp_pot, hks, h_et0)

        ! water balance equation (TODO: h_soil_mean)
        h_soil_new = h_soil0 + (h_inf+h_net_irr+h_pond_i) - h_eva_act - h_perc1 - h_transp_act

        h_pond = 0
        ! surplus check: compare the WB solution with the maximum capacity of the soil layer
        if(h_soil_new>=h_sat)then
            h_pond = h_soil_new-h_sat ! put surplus to the surface (ponding)
            h_soil_new = h_sat
        else if(h_soil_new<=(0.5*h_wp)) then
            h_perc1 = h_perc1 + (h_soil_new-(0.5*h_wp)) ! reduce percolation volume to set v_soil_new = 0.5 * 0.5_wp
            h_soil_new = 0.5*h_wp
            ! TODO: what if not enough water?
        ! TODO: %CG%: when v_soil_new > v_fc --> v_perc1 = v_soil_new -v_fc
        end if

        ! check solution convergence
        h_delta = abs(h_soil_new-h_soil_old)
        if(h_delta<=converg) exit
        h_soil_mean = ((h_soil_new + h_soil_old) * 0.5 + h_soil0) * 0.5
        h_soil_old = h_soil_new
    end do

    ! if no convergence
    if(n == n_max + 1) then
        m = mmax        ! save the counter
        mmax = mmax + 1 ! update the counter
        h_eva_act = 0
        h_eva_pot = 0
        h_perc1 = 0
        n = 0 ! store the number of cycles inside the convergence loop

        do k=1, k_max
            call water_balance_evap_lay(h_net_irr/k_max, h_soil1, h_inf/k_max, h_eva_act_m, h_eva_pot_m, h_perc1_m, h_pond_i, h_pond, &
                & h_transp_act_m, h_transp_pot_m, k_cb, p_day, h_et0/k_max, k_e, k_r,k_s_dry_m,k_s_sat_m, hks_m, kc_max, few, rf_e, & ! RR: add k_r
                & h_rew, h_sat, h_fc, h_wp, h_r, k_sat/k_max , fatt_n, n_iter1, adj_perc_par, mmax,doy)

            h_pond_i = h_pond
            h_eva_act = h_eva_act + h_eva_act_m
            h_eva_pot = h_eva_pot + h_eva_pot_m
            h_transp_act = h_transp_act + h_transp_act_m
            h_transp_pot = h_transp_pot + h_transp_pot_m
            h_perc1 = h_perc1 + h_perc1_m
            n = max(n_iter1, n)
        end do
        h_soil_new = h_soil1
    end if

    h_soil1 = h_soil_new
    n_iter1=n
end subroutine water_balance_evap_lay

! TODO: %AB%: check consistency between water_balance_eva_lay and  water_balance_transp_lay
recursive subroutine water_balance_transp_lay(h_soil2, h_transp_act, h_transp_pot, h_perc2, &
    & h_perc1, k_stress_dry, k_stress_sat, hks, h_eva_pot, h_caprise, h_rise, sr, zr, rf_t, k_cb, p_day, h_et0,h_sat,h_fc,h_wp,h_r, k_sat, &
    & fatt_n, a3 , a4 , b1 , b2, b3, b4, depth_under_rz, &
    & n_iter2, adj_perc_par,caprise_flag,mmax,doy)
    ! water balance of the evapotranspirative layer
    ! recursive: time is divided by 2 in case solution not found
    real(dp), intent(in)::zr            ! depth of the transpirative layer [m]
    real(dp), intent(in)::sr            ! root zone depth [m]
    real(dp), intent(in)::h_perc1       ! percolation volume from the first (above) layer [mm]
    real(dp), intent(in)::h_eva_pot     ! potential evaporation [mm] - (sub)hourly
    real(dp), intent(in)::h_et0         ! reference evapotranspiration [mm]
    real(dp), intent(in)::k_cb          ! base crop coefficient [-]
    real(dp), intent(in)::p_day         ! deplection fraction adjusted for meteorological conditions [-]
    real(dp), intent(in)::rf_t          ! fraction of active roots in transpirative layer [mm]
    real(dp), intent(in)::h_sat         ! water storage at saturation [mm]
    real(dp), intent(in)::h_fc          ! water storage at field capacity [mm]
    real(dp), intent(in)::h_wp          ! water storage at wilting point [mm]
    real(dp), intent(in)::h_r           ! residual water storage [mm]
    real(dp), intent(in)::k_sat         ! hydraulic conductivity at saturation [cm/h]
    real(dp), intent(in)::fatt_n        ! Brooks & Corey curve fitting parameter [-]
    real(dp), intent(in)::a3            ! capillary rise model parameter
    real(dp), intent(in)::a4            ! capillary rise model parameter
    real(dp), intent(in)::b1            ! capillary rise model parameter
    real(dp), intent(in)::b2            ! capillary rise model parameter
    real(dp), intent(in)::b3            ! capillary rise model parameter
    real(dp), intent(in)::b4            ! capillary rise model parameter
    real(dp), intent(in)::depth_under_rz! water table depth under the root zone
    real(dp), intent(in)::adj_perc_par  ! factor that takes into account irrigation method and days spent from last irrigation [-]
    logical, intent(in)::caprise_flag   ! activate (true) capillary rise calculation
    integer, intent(in)::doy            ! current day for debug

    real(dp), intent(inout)::h_soil2    ! soil water content of the layer [mm] - (sub)hourly
    integer, intent(inout)::n_iter2     ! final number of iteration [-]
    integer, intent(inout)::mmax        ! maximum number of iteration [-]

    real(dp), intent(out)::h_transp_act ! effective transpiration [mm] - (sub)hourly
    real(dp), intent(out)::h_transp_pot ! potential transpiration [mm] - (sub)hourly
    real(dp), intent(out)::h_perc2      ! percolation from the second layer (mm) - (sub)hourly
    real(dp), intent(out):: k_stress_dry! water scarcity stress coefficient [-]
    real(dp), intent(out):: k_stress_sat! water saturation stress coefficient [-]
    real(dp), intent(out)::hks          ! water stress coefficient [-]
    real(dp), intent(out)::h_caprise    ! water uplifted due to capillary rise[mm]
    real(dp), intent(out)::h_rise       ! water uplifted due to saturation excess [mm]

    !real(dp)  ::dir_uptake  ! direct uptake of water from water table    ! %EAC%: NOT IMPLEMENTED

    integer, parameter::nmax=20
    integer, parameter::kmax = 2
    real(dp),parameter::converg=0.0001
    real(dp)::h_soil0,h_soil_old,h_soil_mean,h_soil_new,h_delta,h_perc2_max
    integer::n, m, k
    real(dp)::h_caprise_m, h_transp_act_m, h_transp_pot_m,k_s_dry_m,k_s_sat_m, hks_m, h_perc2_m, h_rise_m

    ! init
    h_soil0 = h_soil2
    h_soil_old = h_soil0
    h_soil_mean = h_soil0
    m = 1

    do n=1,nmax
        h_caprise = 0.
        h_rise = 0.

        call calculate_water_stresses(h_soil_mean,h_fc,h_wp,p_day,k_stress_dry,k_stress_sat)

        hks = min(k_stress_dry,k_stress_sat)

        call transpiration(k_cb,rf_t, h_transp_act, h_transp_pot, hks, h_et0)

        ! %AB%: calculate capillary rise if required
        ! TODO: %CG%: add transpiration from the 1st layer
        !%PS%: allowed caprise for rice paddies (it doesn't compromise percolation because cap_rise() returns 0 for very wet soil, and can be helpful when irrigation is turned off)
        if (caprise_flag .and. k_cb > 0._dp) then
            h_caprise = cap_rise(sr, depth_under_rz, &
                & h_soil_mean/(zr*1000), h_fc/(zr*1000), h_wp/(zr*1000), &
                & k_cb * h_et0, h_eva_pot, &   ! replace with total transpiration & h_transp_pot, h_eva_pot,&
                & a3, a4, b1, b2, b3, b4)
        end if

        ! calculate percolation if no capillary rise
        if(h_caprise > 0.) then
            h_perc2 = 0.
        else
            h_perc2 = percolation(adj_perc_par, h_soil_mean, h_r, h_sat, k_sat, fatt_n)
        end if

        ! %CG% add control to limit percolation to the available saturation volume
        ! below the second layer (note that we use the same parameters set from layer 2)
        h_perc2_max = ((h_sat-h_fc)/zr)*depth_under_rz ! maximum water storage between root zone and water table
        h_perc2 = min(h_perc2,h_perc2_max)
        ! water balance equation
        h_soil_new = h_soil0 + h_perc1 - h_transp_act - h_perc2 + h_caprise

        ! check surplus: compare the water balance solution with the storage capacity of the layer
        if (h_soil_new >= h_sat) then
            h_rise = h_soil_new - h_sat   ! excess water moves upward
            h_soil_new = h_sat
        else if (h_soil_new <= h_r) then
            ! limit percolation in order to not go under wilting point %PS%: changed to residual
            h_perc2 = h_perc2 + (h_soil_new - h_r)
            h_soil_new = h_r
        end if

        ! convergence of the solution
        h_delta = abs(h_soil_new-h_soil_old)

        if (h_delta <= converg) exit
        h_soil_mean = ((h_soil_new+h_soil_old)*0.5+h_soil0)*0.5
        h_soil_old = h_soil_new



    end do

    ! if no convergence
    if (n == nmax+1) then
        m = mmax        ! save the counter
        mmax = mmax + 1 ! update the counter
        h_rise = 0
        h_caprise = 0
        h_transp_act = 0
        h_transp_pot = 0
        hks = 0
        h_perc2 = 0
        n = 0
        do k = 1, kmax
            call water_balance_transp_lay(h_soil2, h_transp_act_m, h_transp_pot_m, h_perc2_m, h_perc1/kmax,k_s_dry_m,k_s_sat_m, hks_m, h_eva_pot/kmax, h_caprise_m, &
                & h_rise_m, sr, zr, rf_t, k_cb, p_day, h_et0/kmax, h_sat,h_fc,h_wp,h_r, k_sat/kmax, fatt_n, &
                & a3 , a4 , b1 , b2 , b3, b4 , depth_under_rz, n_iter2, adj_perc_par,caprise_flag,mmax,doy)
            ! %AB%: save all the variable
            h_caprise = h_caprise + h_caprise_m
            h_transp_act = h_transp_act + h_transp_act_m
            h_transp_pot = h_transp_pot + h_transp_pot_m
            h_perc2 = h_perc2 + h_perc2_m
            h_rise = h_rise + h_rise_m

            hks = hks_m ! consistent with those externally calculated
            n = max(n_iter2,n)
        end do
        h_soil_new = h_soil2
    end if

    h_soil2 = h_soil_new
    n_iter2=n

end subroutine water_balance_transp_lay

end module
