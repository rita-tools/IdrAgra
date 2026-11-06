# Project structure

In this section we look at how an IdrAgra working folder is structured.



## Input files

Input files make up the majority of the project's data. By default, they are organized in descriptive folders:

- **{ref}`spatial_data <parameter-inputpath>`**, containing .asc maps, i.e. anything that is spatialized at the {ref}`field <simulation-unit>` level;
- **{ref}`meteo_data <parameter-meteopath>`**, containing weather time series (one file per station);
- **{ref}`landuses <parameter-cropinputsfolder>`**, containing the rotation table and crop-definition files;
- **{ref}`irrmeth_data <parameter-irrmethpath>`**, containing each irrigation method's parameters;
- **{ref}`watsour_data <parameter-watsourpath>`**, containing water-source and scheduled-irrigation files;
- **{ref}`sim_results <parameter-outputpath>`**, containing generated outputs.

These are the default names. Each folder can be changed through its linked setting in `idragra_parameters.txt`.

...

(water-table-input-files)=
### Water table depth

If the user sets {ref}`CapillaryFlag <parameter-capflag>` to true, the model requires one or more .asc files describing the depth of the water table from the soil surface [m] to be placed in the {ref}`spatial_data <parameter-inputpath>` folder, following a strict naming convention:

- `waterdepth.asc` is read at the beginning of the simulation, and is always required.
- `waterdepth_yyyy_<doy>.asc` provides a recurring update on a day of year; `waterdepth_<year>_<doy>.asc` provides a year-specific update. Both forms are optional.

IdrAgra treats the received water table depths as static until they get updated by reading a new daily map. \
It is not necessary to provide a map for each day of the simulation - IdrAgra will simply continue the simulation using the last map it has read.

:::{container} llm-review-note
**LLM-authored draft — review required.** The practical inventory below was drafted from the 2025 manuals, the bundled demo, and the current file-reading code. Review it before relying on it as a complete input specification.
:::

(working-folder-layout)=
## Working-folder layout

IdrAgra locates each group of files through the path settings in `idragra_parameters.txt`. A typical project using the default names contains:

```text
project/
├── idragra_parameters.txt
├── CO2_conc.dat        # annual atmospheric CO2 concentrations
├── weather_stations.dat
├── spatial_data/       # ESRI ASCII grids and rice_soilparam.txt
├── meteo_data/         # one daily weather file per station
├── landuses/           # rotations and crop definitions
├── irrmeth_data/       # irrigation-method list and method files
├── watsour_data/       # source, district, diversion, and schedule files
└── sim_results/        # generated outputs
```

Alternative names work when {ref}`InputPath <parameter-inputpath>`, {ref}`MeteoPath <parameter-meteopath>`, {ref}`CropInputsFolder <parameter-cropinputsfolder>`, {ref}`CropFolder <parameter-cropfolder>`, {ref}`IrrMethPath <parameter-irrmethpath>`, {ref}`WatSourPath <parameter-watsourpath>`, and {ref}`OutputPath <parameter-outputpath>` point to the matching folders. When copying a parameter file between projects, copy the associated folder structure or update these paths.

(spatial-ascii-grids)=
## Spatial ASCII grids

Spatial inputs are ESRI ASCII grids. They must share alignment and resolution and cover the active domain. `domain.asc` defines which cells participate. The required filenames are:

| Group | Required root names |
|---|---|
| Domain | `domain` |
| Layer I water properties | `ThetaI_sat`, `ThetaI_FC`, `ThetaI_WP`, `ThetaI_r`, `Ksat_I`, `N_I` |
| Layer II water properties | `ThetaII_sat`, `ThetaII_FC`, `ThetaII_WP`, `ThetaII_r`, `Ksat_II`, `N_II` |
| Runoff | `slope`, `hydr_cond`, `hydr_group` |
| Land use and station weights | `soiluse`, `meteo_1`, ..., `meteo_N` |
| Irrigation, depending on mode | `irr_meth`, `appl_eff`, `irr_units`, `conv_eff` |
| Capillary rise, when enabled | `waterdepth`, `CapRisePar_a3`, `CapRisePar_a4`, `CapRisePar_b1`, `CapRisePar_b2`, `CapRisePar_b3`, `CapRisePar_b4` |
| Optional | `shapearea`, `irandom` |

Add the `.asc` extension to each root name shown above. Store these files in {ref}`InputPath <parameter-inputpath>`.

A conventional ESRI ASCII header is:

```text
ncols         100
nrows         80
xllcorner     500000
yllcorner     4990000
cellsize      250
NODATA_value  -9999
```

All grids must match the domain's row and column counts, lower-left origin, and cell size. NoData cells in a required parameter grid are excluded from the simulation domain.

:::{container} manual-code-divergence
**Validation gap to review.** IdrAgra does not currently warn when an input grid has a different `cellsize`. Check the resolution before running because IdrAgra does not resample input grids.
:::

:::{container} manual-code-divergence
**Manual/code divergence to review.** The installation manual describes `0` as the value outside `domain.asc`, but zero is treated as an active value. Use the grid's declared `NODATA_value` outside the domain.
:::

Enter slope values in `slope.asc` as percentages. Land-use IDs range from `1` to the number of entries in the crop rotation table. Use irrigation-method ID `0` for no method, or a positive ID listed in the irrigation-method file.

### Time-varying maps

When {ref}`SoilUseVarFlag = T <parameter-soilusevarflag>`, yearly land-use and irrigation-method maps use:

```text
soiluse_<year>.asc
irr_meth_<year>.asc
appl_eff_<year>.asc   # read in Modes 2 and 4 when land use varies
```

The first simulation year must be present. The same naming convention is used when the maps are refreshed at the start of later years.

Water-table depth uses `waterdepth.asc` as its initial map. While {ref}`CapillaryFlag <parameter-capflag>` is enabled, IdrAgra looks for two optional update forms each day:

```text
waterdepth_yyyy_<doy>.asc   # recurring update in every year
waterdepth_<year>_<doy>.asc # update for one specific year
```

If both exist on the same day, the year-specific file takes precedence. A new map remains active until another update is found. Write the day of year as an unpadded integer, for example `waterdepth_2022_91.asc`.

:::{container} manual-code-divergence
**Manual/code divergence to review.** One map table in the installation manual uses `meth_eff_<year>.asc`. IdrAgra expects `appl_eff_<year>.asc`.
:::

(weather-input-files)=
## Weather inputs

{ref}`MeteoFileName <parameter-meteofilename>` points to a root-level station-list file. Its structure is:

```text
StatNum = 2
Table =
FileName X Y
station_west.dat 500250.0 5000750.0
station_east.dat 502000.0 5001000.0
EndTable =
```

`StatNum` must equal {ref}`MeteoStatTotNum <parameter-meteostattotnum>`, and the table must contain exactly that many station records. Coordinates must use the same projected coordinate system as the spatial grids.

Each station file begins with four header lines followed by one row per calendar day:

```text
Id station: 137, location: example
45.444092 102.0
01/01/2021 -> 31/12/2022
T_max T_min P_tot U_max U_min V_med RG_CORR
```

The second line contains latitude in degrees and elevation in metres. The daily columns are maximum and minimum air temperature, precipitation, maximum and minimum relative humidity, wind velocity, and solar radiation. Use a numeric filename stem as the station ID, or provide a positive ID in the first header line. IDs must be unique, and every station series must cover the requested simulation period.

The spatial weights are named with the lowercase root `meteo_1.asc`, ..., `meteo_N.asc`. Windows file lookup is normally case-insensitive, but projects intended to run on Linux should preserve this exact case even though the manual sometimes prints `Meteo_1.asc`.

:::{container} manual-code-divergence
**Manual/code divergence to review.** The installation manual restricts weather filenames to numeric station codes. Names such as `station_west.dat` also work when the first header line contains a positive, unique station ID.
:::

(selected-cell-input)=
## Selected-cell input

The presence of `cells.txt` in the working directory enables selected-cell outputs. The file contains a count followed by cell identifiers and one-based row and column indices:

```text
NCells = 2
Table =
ID Row Col
1 20 60
2 32 41
EndTable =
```

`NCells` must match the number of table rows. These are grid indices, not projected coordinates. The selected cells drive the annual daily-series and cell-information CSV outputs described on the [outputs page](outputs.md).

(crop-input-files)=
## Crop and phenology inputs

IdrAgra reads the rotation table selected by {ref}`SoilUsesFilename <parameter-soilusesfilename>` from {ref}`CropInputsFolder <parameter-cropinputsfolder>`, and reads the referenced crop parameter files from {ref}`CropFolder <parameter-cropfolder>`. A positive integer prefix such as `13_maize.tab` supplies a stable crop ID; files without one receive deterministic generated IDs.

If a referenced crop file starts with `0_`, IdrAgra also uses its first curve row and static parameters for days when a cell is bare. The rotation entry still receives a positive ID. Without a referenced `0_` file, IdrAgra uses its built-in bare-soil values.

For crops with `Adj_flag = 1` (the default), IdrAgra calculates a Kcb correction from the nearest weather station's average climate. The crop file can specify an inclusive day/month interval for the weather average:

```text
KcbCorrectionStart = 01/07
KcbCorrectionEnd = 31/08
```

The interval may cross New Year. Both dates must be supplied together. If neither is present, IdrAgra warns once for the crop and uses 70–95% of the estimated sowing-to-harvest period: sowing is estimated halfway through the allowed window, and harvest uses `HarvestDate_max`. Complete occurrences of the interval in the station series are averaged. If none is complete, IdrAgra warns and uses the available days within incomplete occurrences; if no occurrence overlaps the weather series, it warns and uses the entire series. One additive correction per crop–station pair is calculated using the crop's peak tabulated height and reused each year; daily crop development still follows local weather. `Adj_flag = 0` retains the input Kcb curve.

The correction applies on days from the input curve's start of mid-season when the uncorrected Kcb is greater than 0.45. Crops without a positive-length maximum-Kcb plateau or with peak Kcb at or below 0.45 are not corrected. The phase check uses accumulated GDD and the uncorrected curve, so climate correction cannot move its own eligibility thresholds.

The {ref}`CO2Filename <parameter-co2filename>` file supplies one atmospheric CO2 concentration for each required calendar year. Its path is relative to the working folder unless an absolute path is supplied. For example:

```text
Year CO2
2021 415.0
2022 418.0
```

IdrAgra computes crop development, canopy resistance, and CO2-adjusted water productivity internally. Yield accumulates for the crop in each cell through warmup and across calendar years, then appears in the year of harvest. Output suffixes (`_1`, `_2`) count harvested crops in each cell for that year; a crop not harvested has no yield result.
(soil-moisture-inputs)=
## Initial and final soil moisture

With {ref}`InitialThetaFlag = T <parameter-initialtheta>`, IdrAgra reads two files made from the {ref}`InitialCondition <parameter-initialcondition>` root in {ref}`InitialConditionPath <parameter-initialconditionpath>`:

```text
<InitialCondition>I.asc
<InitialCondition>II.asc
```

For example, `InitialCondition = IC_theta` selects `IC_thetaI.asc` and `IC_thetaII.asc`; omitting the setting uses those same default filenames. Missing values inside the active domain are filled from the corresponding field-capacity map.

With {ref}`InitialThetaFlag = F <parameter-initialtheta>`, IdrAgra starts from field capacity and runs its warm-up year. With {ref}`FinalThetaFlag = T <parameter-finaltheta>`, it writes both layers using {ref}`FinalCondition <parameter-finalcondition>` in {ref}`FinalConditionPath <parameter-finalconditionpath>`.

(cn-class)=
### Crop parameters

...
