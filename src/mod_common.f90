! translate --> OK

module mod_common
use mod_constants, only: dp
use mod_grid
implicit none

type layer_balance_matrices
    real(dp) ,dimension(:,:), allocatable :: thickness      ! layer thickness [m]
    real(dp) ,dimension(:,:), allocatable :: h_soil         ! water storage [mm]
    real(dp) ,dimension(:,:), allocatable :: t_soil         ! volumetric water content [m3/m3]
    real(dp) ,dimension(:,:), allocatable :: h_transp_act   ! actual transpiration [mm]
    real(dp) ,dimension(:,:), allocatable :: h_transp_pot   ! potential transpiration [mm]
    real(dp) ,dimension(:,:), allocatable :: h_perc         ! percolation leaving the layer [mm]
end type layer_balance_matrices

type balance_matrices
    type(layer_balance_matrices)::lyr(2)
    real(dp) ,dimension(:,:), allocatable :: h_raw          ! profile water content when RAW is fully depleted [mm]
    real(dp) ,dimension(:,:), allocatable :: h_irr_thresh   ! profile water content threshold below which general irrigation is triggered [mm]
    real(dp) ,dimension(:,:), allocatable :: h_priv_thresh  ! profile water content threshold below which private-source irrigation is triggered [mm]
    real(dp) ,dimension(:,:), allocatable :: h_eva          ! actual evaporation [mm]
    real(dp) ,dimension(:,:), allocatable :: h_eva_pot      ! potential evaporation [mm]
    real(dp) ,dimension(:,:), allocatable :: h_interc       ! interception [mm]
    real(dp) ,dimension(:,:), allocatable :: h_pond         ! surface ponding [mm]
    real(dp) ,dimension(:,:), allocatable :: h_inf          ! infiltration [mm]
    real(dp) ,dimension(:,:), allocatable :: h_runoff       ! runoff [mm]
    real(dp) ,dimension(:,:), allocatable :: h_eff_rain     ! effective rainfall [mm]
    real(dp) ,dimension(:,:), allocatable :: h_net_av_water ! water available for infiltration after runoff [mm]
    real(dp) ,dimension(:,:), allocatable :: h_gross_av_water!precipitation plus above-canopy irrigation [mm]
    real(dp) ,dimension(:,:), allocatable :: k_s            ! profile water stress coefficient [-]
    real(dp) ,dimension(:,:), allocatable :: depth_under_rz ! water-table depth below root zone [m]
    real(dp) ,dimension(:,:), allocatable :: h_caprise      ! capillary rise into the root zone [mm]
    real(dp) ,dimension(:,:), allocatable :: h_rise         ! rise from layer 2 to layer 1 [mm]
end type balance_matrices

type extensive                                      ! hourly steps of balance terms
! Definition of type extensive
! Stores in distributed form all balance terms that are calculated at hourly step
    real(dp),dimension(:,:),pointer::k_e                 ! actual soil evaporation coefficient (Ke) [-] - hourly
    real(dp),dimension(:,:),pointer::h_eva               ! actual evaporation [mm] - hourly
    real(dp),dimension(:,:),pointer::h_eva_pot           ! potential evaporation [mm] - hourly
    real(dp),dimension(:,:),pointer::k_r                 ! reduction coefficient of evaporation
    real(dp),dimension(:,:),pointer::h_inf               ! infiltration [mm] - hourly
    real(dp),dimension(:,:),pointer::h_perc1             ! percolation from 1st to 2nd layer [mm] - hourly
    real(dp),dimension(:,:),pointer::h_perc2             ! percolation from the 2nd layer to the water table [mm] - hourly
    real(dp),dimension(:,:),pointer::h_pond              ! ponding [mm] - hourly, final
    real(dp),dimension(:,:),pointer::h_transp_act1       ! hourly actual transpiration of 1st layer [mm]
    real(dp),dimension(:,:),pointer::h_transp_pot1       ! hourly potential transpiration of 1st layer [mm]
    real(dp),dimension(:,:),pointer::h_transp_act2       ! hourly actual transpiration of 2nd layer [mm]
    real(dp),dimension(:,:),pointer::h_transp_pot2       ! hourly potential transpiration of 2nd layer [mm]
    real(dp),dimension(:,:),pointer::h_caprise           ! capillary rise  [mm] - hourly
    real(dp),dimension(:,:),pointer::h_rise              ! rise from 2nd to 1st layer [mm] - hourly
    real(dp),dimension(:,:),pointer::h_eff_rain          ! effective rainfall [mm] - hourly
    real(dp),dimension(:,:),pointer::h_net_av_water      ! net available water = effective rainfall + sprinkle irrigation (over which runoff is calculated) - hourly
    real(dp),dimension(:,:),pointer::h_gross_av_water    ! gross available water = effective rainfall + sprinkle irrigation - hourly
end type extensive

type intensive
! Definition of type intensive
! Stores in distributed form all balance terms that are calculated at hourly step that depends on previous iteration
    real(dp),dimension(:,:),pointer::h_soil1         ! water content of evaporative layer [mm] - hourly, h-1 (previous iteration)
    real(dp),dimension(:,:),pointer::h_soil2         ! water content of transpirative layer [mm] - hourly, h-1 (previous iteration)
    real(dp),dimension(:,:),pointer::k_s_dry         ! water scarcity stress coefficient [-] - hourly, h-1 (previous iteration)
    real(dp),dimension(:,:),pointer::k_s_sat         ! water saturation stress coefficient [-] - hourly, h-1 (previous iteration)
    real(dp),dimension(:,:),pointer::k_s             ! water stress coefficient [-] - hourly, h-1 (previous iteration)
    real(dp),dimension(:,:),pointer::h_pond0         ! ponding [mm] - hourly, h-1 (previous iteration)
end type intensive

type hourly
! Definition of type hourly
! Inherits extensive/intensive variables and stores algorithm iterations
    type(extensive)::esten
    type(intensive)::inten
    integer,dimension(:,:),pointer::n_iter1  ! convergence index for water_balance_evap_lay [-] - hourly
    integer,dimension(:,:),pointer::n_iter2  ! convergence index for water_balance_transp_lay [-] - hourly
    integer,dimension(:,:),pointer::n_max1   ! convergence index for water_balance_evap_lay [-] - n. of iterations in an hour
    integer,dimension(:,:),pointer::n_max2   ! convergence index for water_balance_transp_lay - n. of iterations in an hour
end type hourly

type unit_file_scratch
    integer,dimension(:),pointer::dxi
end type unit_file_scratch

type moisture               ! Moisture indicates in this case the relative water content [m^3/m^3]
    type(grid_r)::wp        ! Soil moisture at wilting point
    type(grid_r)::fc        ! Soil moisture at field capacity
    type(grid_r)::old       ! Soil moisture at time step -1
    type(grid_r)::sat       ! Soil moisture at saturation
    type(grid_r)::r         ! Residual soil moisture
end type moisture

type spatial_info
    type(grid_i)::domain                        ! Domain - skipping not simulated soil uses
    type(grid_i)::backup_domain                 ! Domain - all soil uses
    type(grid_i)::soil_use_id                   ! id of soil uses
    type(grid_i)::drainage                      ! drainage (CN) TODO: check the use
    type(grid_i)::irr_unit_id                   ! id of the irrigation units
    type(grid_r)::eff_met                       ! efficiency of the irrigation method TODO: actually it should derive from irrigation parameters
    type(grid_r)::eff_net                       ! efficiency of the distribution network
    type(grid_r)::slope                         ! Slope required by CN correction
    type(moisture), dimension(2) :: theta       ! Soil moisture component for the 1st and 2nd layer
    type(grid_r), dimension(2) :: k_sat         ! Hydralic conductivity for the 1st and 2nd layer
    type(grid_r), dimension(2) :: fact_n        ! Brooks & Corey curve fitting parameter [-]
    type(grid_i)::hydr_gr                       ! hydrological group (CN method)
    type(grid_i)::irr_meth_id                   ! id of the irrigation method
    type(grid_r)::wat_tab                       ! Water table
    type(grid_r)::a3                            ! capillary rise model parameter
    type(grid_r)::a4                            ! capillary rise model parameter
    type(grid_r)::b1                            ! capillary rise model parameter
    type(grid_r)::b2                            ! capillary rise model parameter
    type(grid_r)::b3                            ! capillary rise model parameter
    type(grid_r)::b4                            ! capillary rise model parameter
    type(grid_r),dimension(:),pointer::weight_ws! weights of weather stations
    type(grid_r),dimension(2)::a_perc           ! calibration parameter a for the percolation booster parameter
    type(grid_r),dimension(2)::b_perc           ! calibration parameter b for the percolation booster parameter
    type(grid_r)::h_meth                        ! irrigation volume [mm]
    type(grid_r)::cell_area                     ! area of the calculation cell (vectorialization)
    type(grid_r):: h_maxpond                    ! maximum ponding depth [mm]
    type(grid_i)::irr_starts                    ! doy of the year when irrigation period starts
    type(grid_i)::irr_ends                      ! doy of the year when irrigation period ends
    type(grid_i)::irandom                       ! a map with the number to use to shift crop growing
end type spatial_info

! Store the water content in mm
type vol_mm
    real(dp),dimension(:,:),pointer::h_fc
    real(dp),dimension(:,:),pointer::h_wp
    real(dp),dimension(:,:),pointer::h_sat
    real(dp),dimension(:,:),pointer::rew   ! only for the 1st evaporative layer
    real(dp),dimension(:,:),pointer::h_r
end type vol_mm

type wat_matrix
    type(vol_mm),dimension(2)::layer
    type(vol_mm),dimension(1)::theta2_rice
    real(dp),dimension(:,:),pointer::wat1_rew   ! soil water content at REW TODO: units?
    real(dp),dimension(:,:),pointer::few        ! fraction of soil exposed to evaporation
    real(dp),dimension(:,:),pointer::kc_max     ! maximum value of Kcb
end type wat_matrix

! Store the hydrological properties for paddy field
type soil2_rice
    integer::unit_soil_rice  ! unit to file that contains the parameters for rice
    real(dp)::k_sat_2=10.
    real(dp)::n_2
    real(dp)::theta2_FC
    real(dp)::theta2_R
    real(dp)::theta2_SAT
    real(dp)::theta2_WP
end type soil2_rice

end module
