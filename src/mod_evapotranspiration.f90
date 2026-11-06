module mod_evapotranspiration
use mod_constants, only: dp, pi
implicit none


interface ET_reference
    module procedure ET_reference_sc,ET_reference_mat
end interface

contains

subroutine calculate_water_stresses(h_soil, h_fc, h_wp, p_day, k_stress_dry, k_stress_sat)
    ! calculate stresses
    real(dp), intent(in)::h_soil              ! actual soil content [mm]
    real(dp), intent(in)::h_wp                ! water content at wilting point [mm]
    real(dp), intent(in)::h_fc                ! water content at field capacity [mm]
    real(dp), intent(in)::p_day               ! deplection fraction adjusted for meteorological conditions [-]

    real(dp), intent(out)::k_stress_dry       ! water scarcity stress coefficient[-]
    real(dp), intent(out)::k_stress_sat       ! water saturation stress coefficient[-]
    real(dp) :: raw                           ! readily available water [mm]
    real(dp) :: h_raw                         ! soil water content st readily available water is transpired [mm]

        ! TODO: raw can be computed as bil2%RAW outside hourly cycle
    raw = p_day*(h_fc-h_wp)                  ! FAO56 eq. 83
    h_raw = h_fc - raw
    ! evaluate water stress condition k_stress = 1 -> no stress, k_stress = 0 -> total stress
    ! TODO: consider transpiration reduction while approaching saturation
    if(h_soil>=h_raw) then
        k_stress_dry = 1.0
    else if(h_soil>h_wp)then
        k_stress_dry = (h_soil-h_wp)/(h_raw-h_wp)       ! FAO56 eq. 84
    else ! h_soil <= h_wp
        k_stress_dry = 0.0
    end if

    k_stress_sat = 1.0 ! not implemented

end subroutine

subroutine evaporation(h_soil, h_rew, h_wp, k_c_max, few, k_e_act, k_cb,h_et0,h_eva,h_eva_pot,k_r)
    ! evaporation (daily calculation)

    real(dp), intent(in)::h_soil            ! soil water content [mm]
    real(dp), intent(in)::h_rew             ! ready evaporable water [mm]
    real(dp), intent(in)::h_wp              ! water content in 1st layer at wilting point [mm]
    real(dp), intent(in)::k_c_max           ! upper limit on evaporation [-]
    real(dp), intent(in)::few               ! exposed and wetted soil fraction [-]
    real(dp), intent(in)::k_cb              ! crop Kcb for the selected day [-]
    real(dp), intent(in)::h_et0             ! crop reference evapotranspiration [mm]
    real(dp), intent(out)::k_e_act          ! actual soil evaporation coefficient [-]
    real(dp), intent(out)::h_eva            ! actual crop evaporation [mm]
    real(dp), intent(out)::h_eva_pot        ! potential crop evaporation [mm]
    real(dp), intent(out)::k_r              ! soil evaporation reduction coefficient [-] - %RR%: set to output
    real(dp)::k_e_pot                       ! potential soil evaporation coefficient [-]

    if(h_soil>=h_rew) then                  ! energy limiting stage
        k_r=1
    else if(h_soil>=0.5*h_wp)then           ! falling rate stage
        k_r=(h_soil-0.5*h_wp)/(h_rew-0.5*h_wp)
    else ! h_soil < 0.5*h_wp
        k_r=0
    end if

    !Original equation
    !k_e_act=min(k_r*(k_c_max-k_cb),few*k_c_max) ! FAO56 eq. 71
    !k_e_pot=min((k_c_max-k_cb),few*k_c_max) ! Kr = 1 in potential conditions
    k_e_act = few * k_r * (k_c_max-k_cb) ! %RR%: new approach
    k_e_pot = few * (k_c_max-k_cb) ! %RR%: new approach

    h_eva = k_e_act * h_et0
    h_eva_pot = k_e_pot * h_et0 ! it considers bare soil if there is no crop (FAO56 pag. 146)
end subroutine evaporation

subroutine transpiration(k_cb, Ra, h_transp_act, h_transp_pot, k_stress, h_et0)
    ! transpiration calculation
    ! it doesn't consider transpiration reduction caused by soil anoxy (i.e. if soil is nearly saturated)
    real(dp), intent(in)::k_cb                ! basal crop coefficient [-]
    real(dp), intent(in)::Ra                  ! root ratio of considered layer [mm]
    real(dp), intent(in)::h_et0               ! reference evapotranspiration [mm]
    real(dp), intent(in)::k_stress            ! water stress coefficient [-]
    real(dp), intent(out)::h_transp_act       ! hourly actual transpiration [mm]
    real(dp), intent(out)::h_transp_pot       ! hourly potential transpiration [mm]


    h_transp_pot = k_cb * h_et0
    ! hourly potential transpiration for considered layer [mm]
    ! a = considered layer / b = other layer
    h_transp_pot = h_transp_pot * Ra

    h_transp_act = k_stress * h_transp_pot

end subroutine transpiration

function ET_reference_sc(T_max, T_min, HUM_max, HUM_min, Wind_vel, Rad_sol, lat_ws, alt_ws, res_surf, doy)
    ! calculate reference evapotranspiration considering the elevation
    ! see FAO-56
    ! Note: scalar mode
   integer,intent(in)::doy ! day of the year
    real(dp),intent(in)::T_max, T_min, HUM_max, HUM_min, Wind_vel, Rad_sol, lat_ws, alt_ws
    real(dp),intent(in):: res_surf
    !real(sp),parameter::pi = 3.141592653589793238462643383279502884197_sp
    real(dp),parameter::cost_sol = 0.0820 ! solar contant [MJ m-2 min-1]
    real(dp),parameter::a_s = 0.25, b_s = 0.50    ! Other constants
    real(dp),parameter::albedo = 0.23  ! albedo [-] = 0.23 for grass reference crop
    real(dp),parameter::sigma = 4.903E-9  ! Stefan-Boltzmann constant [MJ K-4 m-2 day-1]

    real(dp)::T_ave, press_atm, gamma
    real(dp)::SVP_max, SVP_min, SVP_ave ! saturation vapour pressure at T_max, T_min and average
    real(dp)::delta_T, delta
    real(dp)::VP_act ! actual vapour pressure
    real(dp)::alat, DR, DL, omegaS, RA_T, RA, RSO, RNS, RNL_T1, RNL_T2, RNL, RN
    real(dp)::W1, W2, W3, ET0_T

    real(dp)::ET_reference_sc

    ! (a) Calculate atmospheric parameters
    T_ave=(T_max+T_min)/2.
    press_atm=101.3*((293-0.0065*alt_ws)/293)**5.26 ! atmospheric pressure at alt_ws m a.s.l. [kPa]
    gamma=0.665E-3*press_atm ! psycometric const [kPa °C-1]
    SVP_max=0.6108*EXP(17.27*T_max/(T_max+237.3)) ! saturation vapour pressure at T_max [kPa]
    SVP_min=0.6108*EXP(17.27*T_min/(T_min+237.3)) ! saturation vapour pressure at T_min [kPa]
    SVP_ave=(SVP_max+SVP_min)/2. ! daily average saturation vapour pressure [kPa]
    delta_T = 4098*(0.6108*EXP(17.27*T_ave/(T_ave+237.3))) ! numerator of slope of saturation vapour pressure curve
    delta = delta_T/(T_ave+237.3)**2 ! slope of saturation vapour pressure curve [kPa °C-1]
    VP_act=(SVP_min*(HUM_max/100)+SVP_max*(HUM_min/100))/2. ! actual vapour pression [kPa]

    ! (b) Calculate net solar radiation
    alat=pi/180*lat_ws ! latitude in radians
    DR=1.+0.033*COS(2*pi*doy/365) ! inverse distance between Earth and Sun
    DL=0.409*SIN((2*pi*doy/365)-1.39) ! solar declination
    omegaS=ACOS(-TAN(alat)*TAN(DL)) ! sunset hour angle
    RA_T=24.0*60.0/pi*cost_sol*DR
    RA=RA_T*(omegaS*SIN(alat)*SIN(DL)+COS(alat)*COS(DL)*SIN(omegaS)) ! extraterrestrial radiation [MJ m-2 day-1]
    RSO=((a_s+b_s)+2E-5*alt_ws)*RA ! clear-sky solar radiation at lat_ws [MJ m-2 h-1]
    RNS=(1-albedo)*Rad_sol ! net solar (shortwave) radiation [MJ m-2 day-1]
    RNL_T1=sigma*((T_max+273.16)**4+(T_min+273.16)**4)/2.
    RNL_T2=RNL_T1*(0.34-0.14*SQRT(VP_act))
    RNL=RNL_T2*(1.35*Rad_sol/RSO-0.35) ! net longwave radiation [MJ m-2 day-1]
    RN=RNS-RNL ! net radiation [MJ m-2 day-1]

    ! (c) Calculate reference evapotranspiration
    W1=0.408*delta*RN ! G is small at daily scale
    W2=gamma*900/(T_ave+273.)*Wind_vel*(SVP_ave-VP_act)
    W3=(delta+gamma*(1.+res_surf*Wind_vel/208))
    ET0_T=(W1+W2)/W3

    ! check for positive ET0
    if (ET0_T>=0) then
        ET_reference_sc=ET0_T
    else
        ET_reference_sc=0.
    end if

    return
end function ET_reference_sc

function ET_reference_mat(T_max, T_min, HUM_max, HUM_min, Wind_vel, Rad_sol, lat_ws, alt_ws, res_surf, doy, imax, jmax)
    ! calculate refeterence evapotranspiration considering the elevation
    ! see FAO-56
    ! Note: matrix mode
    integer,intent(in)::doy ! day of the year
    integer,intent(in)::imax,jmax
    real(dp),dimension(:,:),intent(in)::T_max,T_min,HUM_max,HUM_min,Wind_vel,Rad_sol,lat_ws,alt_ws
    real(dp), intent(in)::res_surf
    ! real(dp),parameter::pi=3.141592653589793238462643383279502884197_sp
    real(dp),parameter::cost_solare=0.0820 ! solar contant [MJ m-2 min-1]
    real(dp),parameter::as=0.25, bs=0.50   ! Other constants
    real(dp),parameter::alpha=0.23  ! albedo [-] = 0.23 for grass reference crop
    real(dp),parameter::sigma=4.903E-9  ! Stefan-Boltzmann constant [MJ K-4 m-2 day-1]

    real(dp),dimension(imax,jmax)::T_ave,press_atm,gamma
    real(dp),dimension(imax,jmax)::SVP_max,SVP_min,SVP_ave  ! saturation vapour pressure at T_max, T_min and average
    real(dp),dimension(imax,jmax)::delta_T,delta
    real(dp),dimension(imax,jmax)::VP_act ! actual vapour pressure
    real(dp),dimension(imax,jmax)::alat,DR,DL,omegaS,RA_T,RA,RSO,RNS,RNL_T1,RNL_T2,RNL,RN
    real(dp),dimension(imax,jmax)::W1,W2,W3,ET0_T

    real(dp),dimension(imax,jmax)::ET_reference_mat

    ! (a) Calculate atmospheric parameters
    T_ave=(T_max+T_min)/2.
    press_atm=101.3*((293-0.0065*alt_ws)/293)**5.26 ! atmospheric pressure at alt_ws m a.s.l. [kPa]
    gamma=0.665E-3*press_atm ! psycometric const [kPa °C-1]
    SVP_max=0.6108*EXP(17.27*T_max/(T_max+237.3)) ! saturation vapour pressure at T_max [kPa]
    SVP_min=0.6108*EXP(17.27*T_min/(T_min+237.3)) ! saturation vapour pressure at T_min [kPa]
    SVP_ave=(SVP_max+SVP_min)/2. ! daily average saturation vapour pressure [kPa]
    delta_T=4098*(0.6108*EXP(17.27*T_ave/(T_ave+237.3)))  ! numerator of slope of saturation vapour pressure curve
    delta=delta_T/(T_ave+237.3)**2 ! slope of saturation vapour pressure curve [kPa °C-1]
    VP_act=(SVP_min*(HUM_max/100)+SVP_max*(HUM_min/100))/2. ! actual vapour pression [kPa]

    ! (b) Calculate net solar radiation
    alat=pi/180*lat_ws ! latitude in radians
    DR=1.+0.033*COS(2*pi*doy/365) ! inverse distance between Earth and Sun
    DL=0.409*SIN((2*pi*doy/365)-1.39) ! solar declination
    omegaS=ACOS(-TAN(alat)*TAN(DL)) ! sunset hour angle
    RA_T=24.0*60.0/pi*cost_solare*DR
    RA=RA_T*(omegaS*SIN(alat)*SIN(DL)+COS(alat)*COS(DL)*SIN(omegaS)) ! extraterrestrial radiation [MJ m-2 day-1]
    RSO=((as+bs)+2E-5*alt_ws)*RA ! clear-sky solar radiation at lat_ws [MJ m-2 h-1]
    RNS=(1-alpha)*Rad_sol ! net solar (shortwave) radiation [MJ m-2 day-1]
    RNL_T1=sigma*((T_max+273.16)**4+(T_min+273.16)**4)/2.
    RNL_T2=RNL_T1*(0.34-0.14*SQRT(VP_act))
    RNL=RNL_T2*(1.35*Rad_sol/RSO-0.35) ! net longwave radiation [MJ m-2 day-1]
    RN=RNS-RNL ! net radiation [MJ m-2 day-1]

    ! (c) Calculate reference evapotranspiration
    W1=0.408*delta*RN ! G is small at daily scale
    W2=gamma*900/(T_ave+273.)*Wind_vel*(SVP_ave-VP_act)
    W3=(delta+gamma*(1.+res_surf*Wind_vel/208))
    ET0_T=(W1+W2)/W3

    ! check for positive ET0
    where (ET0_T>=0)
        ET_reference_mat=ET0_T
    else where
        ET_reference_mat=0.
    end where

    return
end function ET_reference_mat

subroutine calculateDLH(DoY, wsLat,DLH)
    integer, intent(in) :: DoY
    real(dp),intent(in) :: wsLat
    real(dp) :: phi, delta, omega_s
    real(dp), intent(out) :: DLH

    !Computes daylight hours
    phi = pi / 180 * wsLat  ! latitude [rad], FAO56: eq. 22 pag 46
    delta = 0.409 * sin(2 * pi / 365 * DoY - 1.39)  ! solar declination, FAO56: eq. 24 pag 46
    omega_s = acos(-tan(phi) * tan(delta))  ! sunset hour angle [rad], FAO56: eq. 25 pag 46
    DLH = 24 / pi * omega_s  ! daylight hours,  FAO56: eq. 34 pag 48
end subroutine



end module
