# IdrAgra_parameters.txt guide

`idragra_parameters.txt` is the main file which determines how the model behaves.

For example, it contains options and switches to set:
- the simulation mode (e.g. USE or NEED)
- the simulation start and end dates
- whether to simulate capillary uptake or not
- whether to run warmup or not
- the paths to input and output files
- which output files are printed and how frequently

and so on.

In this page we list all of the available settings. Note that most of them are technically optional, due to the model applying internal defaults, which are specified for each entry; settings missing a default are highlighted as required settings.

When launching the IdrAgra exe, the model looks for `idragra_parameters.txt` in that same directory. However, a different path and/or filename can be specified using the `-f` [command line argument](command_line_args.md).

## About the file structure

- Blank lines are ignored by the model and skipped.
- Anything that comes after "#" is a comment and is also ignored.
- Variables can appear in any order, but note that group output switches such as `prt_all` act where they occur, so a later group switch can replace earlier individual choices.
- The accepted structure is "VariableName = value".
- Both the variable name and its value are NOT case-sensitive
- Any number of spaces and tabs is accepted before and after the "=" symbol.
- It is not allowed to set multiple variables on the same line.
- Folder paths are relative to the directory from which IdrAgra is run (usually the exe's location), and must use `\\` as the delimiter.
- If a path includes spaces (not recommended), it must be enclosed in quotes (Path = "a path")
- Boolean variables accept only "T" (true) and "F" (false) as values.

{.parameter-list-group .parameter-list-first}
## Input and output paths

(parameter-outputpath)=
### `OutputPath`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `.\\sim_results\\`
:::

Folder in which simulation results are written.

(parameter-inputpath)=
### `InputPath`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `.\\spatial_data\\`
:::

Folder containing spatialized model inputs.

(parameter-meteopath)=
### `MeteoPath`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `.\\meteo_data\\`
:::

Folder containing meteorological station data.

(parameter-meteofilename)=
### `MeteoFileName`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `weather_stations.dat`
:::

Root-level file listing the meteorological input files.

(parameter-cropinputsfolder)=
### `CropInputsFolder`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `.\\landuses\\`
:::

Folder containing the CropCoef land-use rotation database.

(parameter-soilusesfilename)=
### `SoilUsesFilename`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `soil_uses.txt`
:::

CropCoef rotation table within {ref}`CropInputsFolder <parameter-cropinputsfolder>`.

(parameter-co2filename)=
### `CO2Filename`

:::{container} parameter-summary
**Type:** String · **Default:** `CO2_conc.dat`
:::

Annual atmospheric CO2 concentrations file. It has a `Year CO2` header followed by one `year concentration` row for each meteorological calendar year. IdrAgra uses it to calculate canopy resistance and crop water productivity. Concentration is expressed in ppm.

(parameter-cropfolder)=
### `CropFolder`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `.\\landuses\\crop_parameters\\`
:::

Folder containing CropCoef crop parameter files. A leading positive integer followed by an underscore in a filename is treated as the crop's stable ID. IdrAgra assigns deterministic IDs after the largest explicit ID to files without such a prefix.

(parameter-irrmethpath)=
### `IrrMethPath`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `.\\irrmeth_data\\`
:::

Folder containing irrigation-method files.

(parameter-irrmethfilename)=
### `IrrMethFileName`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `irrmethods.txt`
:::

File listing the irrigation-method input files.

(parameter-watsourpath)=
### `WatSourPath`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `.\\watsour_data\\`
:::

Folder containing water-source input files.

{.parameter-list-group}
## Simulation mode, period and conditions

(parameter-mode)=
### `Mode`

:::{container} parameter-summary
**Type:** Integer  ·  **Default:** `2`
:::

Irrigation simulation mode:\
  0 = no irrigation\
  1 = USE mode\
  2 = NEED mode; field-capacity target\
  3 = NEED mode; fixed irrigation amount\
  4 = Scheduled irrigation: dates and volumes read from file

(parameter-initialtheta)=
### `InitialThetaFlag`

:::{container} parameter-summary
**Type:** Boolean  ·  **Default:** `false`
:::

If true, the initial soil-moisture condition is read from the `InitialConditionPath\\InitialCondition.asc` file.\
If false, IdrAgra begins with all soils at field capacity and runs a complete 365- or 366-day warmup period beginning on the simulation start date. The warmup reuses the corresponding first year of input data before the actual simulation starts.

(parameter-initialconditionpath)=
{.parameter-dependent}
#### `InitialConditionPath`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `.\\spatial_data\\`
:::

(parameter-initialcondition)=
{.parameter-dependent}
#### `InitialCondition`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `IC_thetaI`
:::

(parameter-finaltheta)=
### `FinalThetaFlag`

:::{container} parameter-summary parameter-summary--required
**Type:** Boolean  ·  **Required:** no code default
:::

If true, the final soil_moisture_condition is stored in the `FinalConditionPath\\FinalCondition.asc` file, which can be reused as an initial condition in a subsequent simulation.

(parameter-finalconditionpath)=
{.parameter-dependent}
#### `FinalConditionPath`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `.\\sim_results\\`
:::

(parameter-finalcondition)=
{.parameter-dependent}
#### `FinalCondition`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `FC_thetaI`
:::

### `StartSimulation`

:::{container} parameter-summary
**Type:** Date  ·  **Default:** none
:::

First day of the simulation [dd/mm/yyyy].

### `EndSimulation`

:::{container} parameter-summary
**Type:** Date  ·  **Default:** none
:::

Last day of the simulation [dd/mm/yyyy].

(parameter-capflag)=
### `CapillaryFlag`

:::{container} parameter-summary
**Type:** Boolean  ·  **Default:** `false`
:::

Whether to enable capillary-rise simulation.\
Requires the user to provide input water table depth data (details on format can be found {ref}`here <water-table-input-files>`)

(parameter-soilusevarflag)=
### `SoilUseVarFlag`

:::{container} parameter-summary
**Type:** Boolean  ·  **Default:** `false`
:::

Whether to allow soil use to vary between years.\
If true, the model expects 

{.parameter-list-group}
## Meteorology, land use, and sowing

(parameter-meteostattotnum)=
### `MeteoStatTotNum`

:::{container} parameter-summary
**Type:** Integer  ·  **Default:** `1`
:::

MeteoStatTotNum: total number of meteorological stations.

(parameter-meteostatweightnum)=
### `MeteoStatWeightNum`

:::{container} parameter-summary
**Type:** Integer  ·  **Default:** `1`
:::

MeteoStatWeightNum: number of nearest stations used in the weighted  
meteorological/phenological calculations.

### `InterpolateTemperature`

:::{container} parameter-summary
**Type:** Boolean  ·  **Default:** `true`
:::

Whether simulation cells receive interpolated weather data (T, default&recommended) or the closest station's value (F, sometimes useful for rainfall).

### `InterpolateRain`

:::{container} parameter-summary
**Type:** Boolean  ·  **Default:** `true`
:::

### `InterpolateHumidity`

:::{container} parameter-summary
**Type:** Boolean  ·  **Default:** `true`
:::

### `InterpolateWind`

:::{container} parameter-summary
**Type:** Boolean  ·  **Default:** `true`
:::

### `InterpolateRadiation`

:::{container} parameter-summary
**Type:** Boolean  ·  **Default:** `true`
:::

### `RandSowDaysSym`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `symmetric`
:::

RandSowDaysSym: shape of the sowing-date randomization window.

### `RandSowDaysWind`

:::{container} parameter-summary
**Type:** Integer  ·  **Default:** `0`  ·  **Unit:** days
:::

RandSowDaysWind: half-width or extent, in days, of the sowing-date randomization window.

### `AggressiveSowing`

:::{container} parameter-summary
**Type:** Boolean · **Default:** `false`
:::

When `T`, a standing crop may be harvested early so the next crop in the rotation can be sown in its window, respecting a baresoil interval. If several sowing windows are open, the one that opened earliest is selected.
When `F`, the standing crop is harvested at its normal GDD or maximum harvest-date limit; the next crop may miss its sowing window.

### `ReadCropMaps`

:::{container} parameter-summary
**Type:** Boolean · **Default:** `false`
:::

When `T`, dated `crop_id_<year>_<doy>.asc` maps in `InputPath` prescribe the crop occupying each cell. Crop ID `0` is bare soil and positive IDs are the numeric prefixes of loaded crop-definition files. A missing daily map retains the most recently read values. Changes from `0` to a crop sow it; changes from a crop to `0` harvest it; and changes between positive IDs harvest the old crop and sow the new one at the start of that day.

### `Repeatable`

:::{container} parameter-summary
**Type:** Boolean  ·  **Default:** `true`
:::

Repeatable: use a repeatable random sequence, allowing identical results  
across equivalent runs [T/F].

### `Forecast_day`

:::{container} parameter-summary
**Type:** Integer  ·  **Default:** `5`
:::

Forecast_day: forecast horizon or reference day used by forecast-related  
calculations.

(parameter-periodic-output)=
{.parameter-list-group}
## Periodic output

(parameter-monthlyflag)=
### `MonthlyFlag`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `monthly`
:::

MonthlyFlag: periodic-output schedule. In this demo, "monthly" requests  
monthly output. WeekDay and StartDate/EndDate/DeltaDate below are inactive  
unless a weekly or periodic schedule is selected.

### `WeekDay`

:::{container} parameter-summary
**Type:** String or Integer  ·  **Default:** `monday`
:::

WeekDay: used only when MonthlyFlag selects a weekly schedule.

### `StartDate`

:::{container} parameter-summary
**Type:** Integer  ·  **Default:** `10`
:::

StartDate / EndDate: used only when MonthlyFlag selects a periodic schedule;  
first and last day of year [1...366] included in that schedule.

### `EndDate`

:::{container} parameter-summary
**Type:** Integer  ·  **Default:** `100`
:::

### `DeltaDate`

:::{container} parameter-summary
**Type:** Integer  ·  **Default:** `30`  ·  **Unit:** d
:::

DeltaDate: used only with a periodic schedule; interval between outputs [d].

{.parameter-list-group}
## Soil-water, runoff, and layer parameters

### `01q_eva`

:::{container} parameter-summary
**Type:** Real  ·  **Default:** `0.575118`
:::

Lower and upper Ksat calibration breakpoints for the evaporative layer. IdrAgra does not calculate these percentiles: the supplied values are used directly to interpolate the irrigation-related percolation-booster coefficients. They affect irrigated simulations (Modes 1...4).
01q_eva: lower breakpoint; 09q_eva: upper breakpoint.

### `09q_eva`

:::{container} parameter-summary
**Type:** Real  ·  **Default:** `8.026400`
:::

### `01q_trasp`

:::{container} parameter-summary
**Type:** Real  ·  **Default:** `0.472116`
:::

Lower and upper Ksat calibration breakpoints for the transpirative layer.
As above, these are direct calibration inputs rather than statistics calculated by IdrAgra.
01q_trasp: lower breakpoint; 09q_trasp: upper breakpoint.

### `09q_trasp`

:::{container} parameter-summary
**Type:** Real  ·  **Default:** `7.706101`
:::

### `zEvap`

:::{container} parameter-summary
**Type:** Real  ·  **Default:** `0.10`  ·  **Unit:** m
:::

zEvap: evaporative-layer depth [m].

### `zRoot`

:::{container} parameter-summary
**Type:** Real  ·  **Default:** `0.90`  ·  **Unit:** m
:::

zRoot: transpirative/root-zone depth [m].

### `LambdaCN`

:::{container} parameter-summary
**Type:** Real  ·  **Default:** `0.2`  ·  **Unit:** -
:::

LambdaCN: initial-abstraction ratio used by the Curve Number runoff  
formulation [-].

### `lim_prec`

:::{container} parameter-summary
**Type:** Real  ·  **Default:** `5.0`  ·  **Unit:** mm
:::

lim_prec: minimum precipitation threshold used by rainfall/runoff  
calculations [mm].

(parameter-h-maxpond)=
### `h_maxpond`

:::{container} parameter-summary
**Type:** Real  ·  **Default:** `0.0D0`  ·  **Unit:** mm
:::

h_maxpond: general/out-of-season maximum surface-ponding depth [mm].
During the irrigation season, irrigation-method values override this value.

(parameter-fc-ratio)=
### `fc_ratio`

:::{container} parameter-summary
**Type:** Real  ·  **Default:** `1.0D0`  ·  **Unit:** -
:::

fc_ratio: field-capacity target multiplier [-]. Used only in Mode 2.

{.parameter-list-group}
## Irrigation season and source files

(parameter-startirrseason)=
### `StartIrrSeason`

:::{container} parameter-summary
**Type:** Integer  ·  **Default:** `91`
:::

StartIrrSeason / EndIrrSeason: default irrigation-method season [1...366].
Values specified by an individual irrigation-method file override these.

(parameter-endirrseason)=
### `EndIrrSeason`

:::{container} parameter-summary
**Type:** Integer  ·  **Default:** `304`
:::

(parameter-watsources-fn)=
### `WatSources_fn`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `watsources.txt`
:::

Water-source and irrigation-network input filenames. Most source/diversion files are used by Mode 1; sched_irr_fn is used only by Mode 4.

### `mon_sources_i_div_fn`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `monit_sources_i.txt`
:::

### `mon_sources_ii_div_fn`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `monit_sources_ii.txt`
:::

### `int_reuse_div_fn`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `int_reuse.txt`
:::

### `cr_sources_list_fn`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `cr_sources.txt`
:::

(parameter-irrdistr-list-fn)=
### `IrrDistr_list_fn`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `irr_districts.txt`
:::

(parameter-sched-irr-fn)=
### `sched_irr_fn`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `scheduled_irrigation.txt`
:::

(parameter-output-controls)=
{.parameter-list-group}
## Output controls

Decide which output files

### `prt_all`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `y`
:::

prt_all: master switch for the complete standard output set [y/n].
Individual switches below may be used to enable selected outputs when  
prt_all = n.

### `prt_annual`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `y`
:::

prt_annual: print annual summaries [y/n].

### `prt_yield`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `y`
:::

prt_yield: print crop-yield output [y/n].

### `prt_stp_irr`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `y`
:::

Time-step output switches [y/n].
irrigation

### `prt_stp_cap_rise`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `y`
:::

capillary rise

### `prt_stp_deep_perc`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `y`
:::

deep percolation

### `prt_stp_et_act`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `y`
:::

actual evapotranspiration

### `prt_stp_et_pot`

:::{container} parameter-summary
**Type:** String  ·  **Default:** `y`
:::

potential evapotranspiration

### `prt_step`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_debug`

:::{container} parameter-summary
**Type:** String
**Default:** `n`
:::

### `prt_cell_all`

:::{container} parameter-summary
**Type:** String
**Default:** `n`
:::

### `prt_stp_rain`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_stp_transp_act`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_stp_transp_pot`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_stp_irr_loss`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_stp_irr_nm_priv`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_stp_irr_nm_col`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_stp_runoff`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_dbg_eva_act`

:::{container} parameter-summary
**Type:** String
**Default:** `n`
:::

### `prt_dbg_eff_rain`

:::{container} parameter-summary
**Type:** String
**Default:** `n`
:::

### `prt_dbg_perc1`

:::{container} parameter-summary
**Type:** String
**Default:** `n`
:::

### `prt_dbg_perc2`

:::{container} parameter-summary
**Type:** String
**Default:** `n`
:::

### `prt_dbg_h_soil1`

:::{container} parameter-summary
**Type:** String
**Default:** `n`
:::

### `prt_dbg_h_soil2`

:::{container} parameter-summary
**Type:** String
**Default:** `n`
:::

### `prt_yr_rain`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_rain_crop_season`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_irr`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_irr_loss`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_eva_act_crop_season`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_eva_pot_crop_season`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_transp_act`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_transp_pot`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_runoff`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_net_flux_gw`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_total_eff`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_n_irr_events`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_h_irr_mean`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_biomass_pot`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_yield_pot`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_yield_act`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_T_act_sum`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_T_pot_sum`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_f_WS_stage`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_f_WS`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_f_HS`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_f_HS_sum`

:::{container} parameter-summary
**Type:** String
**Default:** `y`
:::

### `prt_yr_dbg_eva_act_tot`

:::{container} parameter-summary
**Type:** String
**Default:** `n`
:::

### `prt_yr_dbg_rain_eff`

:::{container} parameter-summary
**Type:** String
**Default:** `n`
:::

### `prt_yr_dbg_iter1`

:::{container} parameter-summary
**Type:** String
**Default:** `n`
:::

### `prt_yr_dbg_iter2`

:::{container} parameter-summary
**Type:** String
**Default:** `n`
:::

### `prt_cell_convergence`

:::{container} parameter-summary
**Type:** String
**Default:** `n`
:::

### `prt_cell_evaporation`

:::{container} parameter-summary
**Type:** String
**Default:** `n`
:::

### `prt_cell_runoff`

:::{container} parameter-summary
**Type:** String
**Default:** `n`
:::

### `prt_cell_et0`

:::{container} parameter-summary
**Type:** String
**Default:** `n`
:::

### `prt_debug_out`

:::{container} parameter-summary
**Type:** String
**Default:** `n`
:::

### `prt_init_cond`

:::{container} parameter-summary
**Type:** String
**Default:** `n`
:::
