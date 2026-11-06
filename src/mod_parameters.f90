module mod_parameters
use mod_date, only: date
use mod_utility, only: lower_case, string_to_integers
use mod_constants
implicit none

integer, parameter :: output_monthly = 0
integer, parameter :: output_weekly = 1
integer, parameter :: output_periodic = 2

type simulation
    character(len=200) :: path = '.\\sim_results\\'                     ! path of output
    character(len=200) :: meteo_path = '.\\meteo_data\\'                ! path of meteo tables
    character(len=200) :: ws_list_fn = 'weather_stations.dat'           ! filename (incl. path) of the weather station list
    character(len=200) :: crop_inputs_path = '.\\landuses\\'            ! Path to CropCoef soil-use inputs
    character(len=200) :: co2_conc_fn = 'CO2_conc.dat'                  ! filename (incl. path) of the CO2 concentration series
    character(len=200) :: soil_uses_fn = 'soil_uses.txt'                ! Crop rotation table
    character(len=200) :: crop_parameters_path = '.\\landuses\\crop_parameters\\' ! Path to crop definition files
    character(len=200) :: irr_met_path = '.\\irrmeth_data\\'            ! Path to irrigation methods folder
    character(len=200) :: irr_met_list_fn = 'irrmethods.txt'            ! Filename of the list of irrigation methods
    character(len=200) :: watsour_path = '.\\watsour_data\\'            ! Path to the water sources folder
    character(len=200) :: watsources_fn = 'watsources.txt'              ! Filename of water sources distribution
    character(len=200) :: mon_sources_i_div_fn = 'monit_sources_i.txt'  ! Filename of monitored sources (i) daily diversions
    character(len=200) :: mon_sources_ii_div_fn = 'monit_sources_ii.txt'! Filename of monitored sources (ii) daily diversions
    character(len=200) :: int_reuse_div_fn = 'int_reuse.txt'            ! filename of internal reuse daily diversions
    character(len=200) :: cr_sources_list_fn = 'cr_sources.txt'         ! filename of collective runtime sources list
    character(len=200) :: irrdistr_list_fn = 'irr_districts.txt'        ! filename of public irrigation districts' list
    character(len=200) :: sched_irr_fn = 'scheduled_irrigation.txt'     ! filename of scheduled irrigation file
    character(len=200) :: input_path = '.\\spatial_data\\'              ! Path to spatial data directory (contains most .asc files)
    character(len=200) :: initial_condition = '.\\spatial_data\\'       ! path to initial condition input file
    character(len=200) :: final_condition = '.\\sim_results\\'          ! path to final condition output file
    character(len=255) :: domain_fn = 'domain'                          ! Name of the domain .asc file
    character(len=255) :: thetaI_FC_fn = 'ThetaI_FC'
    character(len=255) :: thetaII_FC_fn = 'ThetaII_FC'
    character(len=255) :: thetaI_WP_fn = 'ThetaI_WP'
    character(len=255) :: thetaII_WP_fn = 'ThetaII_WP'
    character(len=255) :: thetaI_r_fn = 'ThetaI_r'
    character(len=255) :: thetaII_r_fn = 'ThetaII_r'
    character(len=255) :: thetaI_SAT_fn = 'ThetaI_sat'
    character(len=255) :: thetaII_SAT_fn = 'ThetaII_sat'
    character(len=255) :: slope_fn = 'slope'
    character(len=255) :: dren_fn = 'hydr_cond'
    character(len=255) :: hydr_group_fn = 'hydr_group'
    character(len=255) :: ksat_I_fn = 'Ksat_I'
    character(len=255) :: ksat_II_fn = 'Ksat_II'
    character(len=255) :: n_I_fn = 'N_I'
    character(len=255) :: n_II_fn = 'N_II'
    character(len=255) :: thetaI_0_fn = 'IC_thetaI'
    character(len=255) :: thetaII_0_fn = 'IC_thetaII'
    character(len=255) :: thetaI_end_fn = 'FC_thetaI'
    character(len=255) :: thetaII_end_fn = 'FC_thetaII'
    character(len=255) :: soil_prop_x_rice_fn = 'rice_soilparam.txt'
    character(len=255) :: eff_irr_fn = 'appl_eff'
    character(len=255) :: eff_net_fn = 'conv_eff'
    character(len=255) :: irr_units_fn = 'irr_units'
    character(len=255) :: id_irr_meth_fn = 'irr_meth'
    character(len=255) :: wat_table_fn = 'waterdepth'
    character(len=255) :: ParRisCap_a3_fn = 'CapRisePar_a3'
    character(len=255) :: ParRisCap_a4_fn = 'CapRisePar_a4'
    character(len=255) :: ParRisCap_b1_fn = 'CapRisePar_b1'
    character(len=255) :: ParRisCap_b2_fn = 'CapRisePar_b2'
    character(len=255) :: ParRisCap_b3_fn = 'CapRisePar_b3'
    character(len=255) :: ParRisCap_b4_fn = 'CapRisePar_b4'
    character(len=255) :: soiluse_fn = 'soiluse'
    character(len=255) :: meteoweight_fn = 'meteo'
    character(len=255) :: shapearea_fn = 'shapearea'
    character(len=255) :: irandom_fn = 'irandom'

    integer :: step_out = output_monthly        ! periodic-output schedule
    integer :: mode = 2                         ! type of simulation:
                                                ! 0 = without irrigation, 1 = USE mode, 2 = NEED mode at field capacity, 3 = NEED mode at fixed volume, 4 = scheduled irrigation
    logical :: f_soiluse = .false.              ! land use change between years (true) otherwise false
    integer :: weekly_output_weekday = 1        ! day of weekly output (1 = Monday, ..., 7 = Sunday)
    integer :: out_step_start = 10     ! first day included in custom-period output
    integer :: out_step_end = 100      ! last day included in custom-period output
    integer :: out_step_days = 30          ! length of each custom output period [days]
    integer :: start_year                       ! first year of simulation
    integer :: sim_years                        ! number of years for the simulation
    integer :: meteo_years                      ! number of available weather time series
    integer, dimension(:), allocatable :: days_in_year ! number of days in each simulation year
    logical :: f_init_wc = .false.              ! if true, use user provided values otherwise calculate from first year run
    logical :: f_theta_out                      ! id true, write final soil moisture condition
    integer :: rand_seed = -999                 ! seed to initialize the random number generator
    logical :: rand_symmetry = .true.           ! states if the randomization of emergence date is symmetric [-n,+n] or asymmetric [0, 2n]
    integer :: sowing_range = 0                 ! range of variability of the sowing day [days]
    logical :: aggressive_sowing = .false.      ! end a standing crop early to make room for the next rotation crop
    logical :: read_crop_maps = .false.         ! prescribe crop occupancy from crop_id_<year>_<doy>.asc maps
    logical :: repeatable = .true.              ! if true, al random generated are repeated over times
    integer :: imax = 1                         ! maximum number of rows in the raster map
    integer :: jmax = 1                         ! maximum number of columns in the raster map
    real(dp) :: cell_size = 250                 ! lateral dimension of the square cell of the raster map
    real(dp) :: x0 = 1.0                        ! x coordinate of the reference corner
    real(dp) :: y0 = 1.0                        ! y coordinate of the reference corner
    integer :: n_weather_stations = 1           ! Total number of weather stations
    integer :: n_lus = 0                        ! Number of land uses in the imported crop rotation table
    integer :: n_crops = 1                      ! max number of crops per year
    logical :: f_cap_rise = .false.             ! if true, capillary rise is calculated
    logical :: use_modflow_coupling = .false.   ! if true, exchange groundwater fluxes with MODFLOW
    logical :: f_shapearea = .false.            ! if true, use shapes area (for vectorialization)
    logical :: f_irandom                        ! if true, use user defined random values
    integer :: n_interpolation_stations = 1     ! Number of nearby weather stations used to interpolate each cell's weather & phenology
    logical :: interpolate_temp = .true.        ! if false, each cell's temperature is equal to the closest station's (not interpolated)
    logical :: interpolate_rain = .true.        ! if false, each cell's precipitation is equal to the closest station's (not interpolated)
    logical :: interpolate_hum = .true.         ! if false, each cell's humidity is equal to the closest station's (not interpolated)
    logical :: interpolate_wind = .true.        ! if false, each cell's wind is equal to the closest station's (not interpolated)
    logical :: interpolate_rad = .true.         ! if false, each cell's radiation is equal to the closest station's (not interpolated)
    integer :: start_irr_season = 91            ! start irrigation season [doy]
    integer :: end_irr_season = 304             ! end irrigation season [doy]
    integer :: n_irr_meth                       ! number of irrigation methods
    logical :: f_out_cells = .false.            ! if true, print outputs file for control points (aka cells)
    real(dp),dimension(:),allocatable :: res_canopy ! annual canopy resistance derived from CO2
    real(dp),dimension(:),allocatable :: co2_conc ! annual atmospheric CO2 [ppm]
    real(dp), dimension(2,2) :: quantiles = reshape([0.575118, 0.472116, 8.026400, 7.706101], [2,2])
    real(dp) :: lambda_cn = 0.2                 ! lambda parameters for curve number [-]
    real(dp) :: h_prec_lim = 5.0                ! minimum meaningful precipitation [mm]

    type(date) :: start               ! defaults to the weather-series start date
    type(date) :: end                 ! defaults to the weather-series end date
    integer :: forecast_day = 5                 ! number of days to use to cumulate precipitation
    real(dp) :: h_maxpond = 0.0D0               ! overall maximum pond (mm). It will replaced by irrigation method
    real(dp) :: fc_ratio = 1.0D0                ! fraction of FC to fill with irrigation

    ! Standard simulation outputs are enabled by default; diagnostic outputs are opt-in.
    character :: prt_stp_rain='y'
    character :: prt_stp_transp_act='y'
    character :: prt_stp_transp_pot='y'
    character :: prt_stp_irr='y'
    character :: prt_stp_irr_loss='y'
    character :: prt_stp_cap_rise='y'
    character :: prt_stp_irr_nm_priv='y'
    character :: prt_stp_irr_nm_col='y'
    character :: prt_stp_deep_perc='y'
    character :: prt_stp_runoff='y'
    character :: prt_stp_et_pot='y'
    character :: prt_stp_et_act='y'
    character :: prt_dbg_eva_act='n'
    character :: prt_dbg_eff_rain='n'
    character :: prt_dbg_perc1='n'
    character :: prt_dbg_perc2='n'
    character :: prt_dbg_h_soil1='n'
    character :: prt_dbg_h_soil2='n'
    character :: prt_yr_rain='y'
    character :: prt_yr_rain_crop_season='y'
    character :: prt_yr_irr='y'
    character :: prt_yr_irr_loss='y'
    character :: prt_yr_eva_act_crop_season='y'
    character :: prt_yr_eva_pot_crop_season='y'
    character :: prt_yr_transp_act='y'
    character :: prt_yr_transp_pot='y'
    character :: prt_yr_runoff='y'
    character :: prt_yr_net_flux_gw='y'
    character :: prt_yr_total_eff='y'
    character :: prt_yr_n_irr_events='y'
    character :: prt_yr_h_irr_mean='y'
    character :: prt_yr_biomass_pot='y'
    character :: prt_yr_yield_pot='y'
    character :: prt_yr_yield_act='y'
    character :: prt_yr_T_act_sum='y'
    character :: prt_yr_T_pot_sum='y'
    character :: prt_yr_f_WS_stage='y'
    character :: prt_yr_f_WS='y'
    character :: prt_yr_f_HS='y'
    character :: prt_yr_f_HS_sum='y'
    character :: prt_yr_dbg_eva_act_tot='n'
    character :: prt_yr_dbg_rain_eff='n'
    character :: prt_yr_dbg_iter1='n'
    character :: prt_yr_dbg_iter2='n'

    character :: prt_all = 'y'
    character :: prt_step = 'y'
    character :: prt_annual = 'y'
    character :: prt_yield = 'y'
    character :: prt_debug = 'n'
    character :: prt_cell_all = 'n'

    ! set options for cell outputs
    character :: prt_cell_convergence = 'n'
    character :: prt_cell_evaporation = 'n'
    character :: prt_cell_runoff = 'n'
    character :: prt_cell_et0 = 'n'

    ! set other print options
    character :: prt_debug_out = 'n'

    character :: prt_init_cond = 'n'


end type simulation

type layer_depth
    real(dp) :: ze_fix = 0.10 ! depth of the evaporative (1st from top) layer [m]
    real(dp) :: zr_fix = 0.90 ! depth of the transpirative (2nd from top) layer [m] (used when crop is missing)
end type layer_depth

type par_method
    integer :: id                               ! id of the irrigation method, linked to the irrigation input map
    real(dp) :: h_irr                           ! irrigation depth [mm]
    real(dp) :: irr_th_ms                        ! threshold coefficient for the activation of irrigation from monitored source of need mode
    real(dp) :: irr_th_unm                       ! threshold coefficient for the activation of irrigation from unmonitored source of need mode
    real(dp) :: f_wet                           ! fraction of wetted soil [-]
    real(dp) :: a_min = 0.0D0                   ! min value of the "a" parameter for percolation model
    real(dp) :: a_max = 0.0D0                   ! max value of the "a" parameter for percolation model
    real(dp) :: b_min = 0.0D0                   ! min value of the "b" parameter for percolation model
    real(dp) :: b_max = 0.0D0                   ! max value of the "b" parameter for percolation model

    ! irrigation losses estimation model: losses=a_loss+b_loss x wind_vel+c_loss x T_mean
    real(dp) :: a_loss
    real(dp) :: b_loss
    real(dp) :: c_loss

    integer :: f_interception                   ! if 1 (true), irrigation is above the canopy
    real(dp),dimension(:),pointer::freq

    integer :: irr_starts = 1                    ! irrigation period can start from
    integer :: irr_ends  = 366                   ! irrigation period will end to

    real(dp) :: h_maxpond = 0.0D0            ! maximum ponding depth

end type par_method

type par_irr_event
    real(dp) :: f_w                                 ! wetted fraction
    type(par_method),dimension(:),pointer::met      ! irrigation method parameter
    real(dp) :: h_irr                               ! water application depth [mm]
    real(dp) :: alpha_raw                           ! fraction of RAW at which the irrigation occurs
    ! for USE mode only
    real(dp) :: alpha_raw_uc                        ! fraction of RAW at which the irrigation occurs for unmonitored collective water sources
    real(dp) :: expl_fact                           ! exploration factor: multiply the number of cells potentially irrigable
end type par_irr_event

type water_source
    integer :: n_withdrawals = 0                ! number of withdrawals for each water sources
    logical :: f_exists                         ! if true, allocate the variable
    logical :: f_allocate_timeserie             ! if true, allocate the array
end type water_source

type uc_act_rule                                      ! activation rule for unmonitored collective water sources
    integer :: id_well_group                          ! id of the group of wells
    real(dp) :: min_act_trsld                         ! minimum activation threshold
    real(dp), dimension(3) :: flow_rate               ! activation flow rate
    real(dp), dimension(3) :: act_trsld               ! activation threshold
end type uc_act_rule

type parameters
    type(simulation) :: sim
    integer :: n_bac
    type(layer_depth) :: depth
    type(par_irr_event) :: irr
    integer :: n_dotaz = 0
    type(water_source) :: ms_i
    type(water_source) :: ms_ii
    type(water_source) :: intreu
    type(water_source) :: cr
    type(uc_act_rule),dimension(:),pointer :: uc_act_rules
    real(dp),dimension(:),pointer::fet0
end type parameters

type water_sources_table
    integer :: id_irr_unit              ! id of the irrigation unit served by the water source
    character(len=10) :: id_wat_src     ! id of the water source which the water duty refers to
    integer :: type_id                  ! type of the water source, (1) monitored sources i, (2) monitored sources ii, (3) internal reuse, (4) collective unmonitored sources
    real :: duty_frc                    ! Rate of the water duty
    integer :: irr_unit_idx             ! column index in the irrigation units variable
    integer :: wat_src_idx              ! column index in the discharge file
end type water_sources_table

type monitored_sources_table
    ! store monitored sources parameters and daily data
    real(dp),dimension(:),pointer::q_nom                ! list of nominal discharge [m3/s]
    character(len=10),dimension(:),pointer::wat_src_id  ! list of water sources id (name of max 10 characters)
    real(dp),dimension(:,:),pointer::q_daily            ! matrix of daily discharges (days x num water sources) [m3/s]
    integer::unit                                       ! unit associated to the input file
    type(date)::start                                   ! start date of the time serie
    type(date)::finish                                  ! end date of the time serie
end type monitored_sources_table

type unmonitored_sources_table
    ! store unmonitored collective sources parameters
    real(dp),dimension(:),pointer::q_max                ! list of maximum discharge value [m3/s]
    real(dp),dimension(:),pointer::q_nom                ! list of nominal discharge value [m3/s]
    character(len=10),dimension(:),pointer::wat_src_id  ! list of water sources id (name of max 10 characters)
    integer,dimension(:),pointer::unit                  ! unit associated to the input file
end type unmonitored_sources_table

type source_info
! collect all the different water sources
    type(monitored_sources_table)::mn_src_tbl1              ! monitored sources (i)
    type(monitored_sources_table)::mn_src_tbl2              ! monitored sources (ii)
    type(monitored_sources_table)::int_reuse_tbl            ! internal reuse
    type(unmonitored_sources_table)::unm_src_tbl            ! unmonitored water sources
end type source_info

type irr_units_table
    ! store all the data referred to th irrigation units
    integer::id                             ! irrigation unit id
    integer::n_cells                        ! number of cells belonging to the irrigation units
    integer :: last_cell_id = 0             ! id of the latest irrigated cell
    real(dp)::int_distr_eff                 ! internal water distribution efficiency
    real(dp)::h_irr_mean                    ! average irrigation height in the irrigation unit
    real(dp),dimension(4)::q_act_fld        ! discharge available at cell
    real(dp),dimension(4)::q_pot_fld        ! potential discharge available at cell
    real(dp)::q_nom                         ! potential discharge
    real(dp)::q_day                         ! actual daily discharge
    real(dp)::q_surplus                     ! discharge lost at the end of the rotation or the end of the irrigation season
    real(dp)::q_rem                     ! discharge available the following day
    real(dp)::q_un_priv                     ! discharge extracted from unmonitored private sources (e.g. farm wells)
    real(dp)::q_un_coll                     ! discharge extracted from unmonitored collective sources (e.g. public wells)
    integer::n_irrigated_cells              ! number of cell irrigated in the current day
    integer::f_un_priv                      ! flag for activate irrigation from unmonitored private sources (e.g. farm wells)
    real(dp)::n_irrigable_cells             ! number of cell potentially irrigable calculated as v_nom/SUM(v_irr_cells)
    !real(dp)::a_raw                        ! NOT USED: activation threshold for monitored water sources
    !real(dp)::a_raw_priv                   ! NOT USED: activation threshold for private water sources
    real(dp)::f_explore                     ! multiplicative factor to set the number of cells potentially irrigable in a day
    !!! %AB% <------- new variable for the rotation
    real::wat_shift                     ! rotation length (in days or fraction of day)
    integer::n_day                      ! number of cells irrigable in a day (related to the rotation)
end type irr_units_table

type scheduled_irrigation
    integer::irr_unit_id               ! id of the irrigation unit
    integer::year                      ! year of irrigation
    integer::doy                       ! day of the year [1-->365/6]
    real(dp)::h_irr                    ! water depth in each irrigation
end type

end module mod_parameters
