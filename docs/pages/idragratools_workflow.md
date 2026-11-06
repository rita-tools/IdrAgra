# Preparing a project with IdrAgraTools

:::{container} llm-review-note
**LLM-authored draft — review required.** This whole page is an LLM-written summary of the February 2025 IdrAgraTools v4 user manual. The exported files were compared with this repository's current demo and readers, but the QGIS interface itself is not part of this repository and could not be code-verified here.
:::

IdrAgraTools is a QGIS plugin for assembling spatial, tabular, and time-series inputs, exporting them into IdrAgra's text/raster formats, launching the preprocessing and simulation executables, and importing results for analysis.

## Before creating a project

The v4 manual assumes basic QGIS familiarity and an installed IdrAgraTools plugin. Use a **projected coordinate reference system** whose map units are metres: the manual explicitly states that geographic longitude/latitude coordinate systems are not supported for the model domain.

The plugin stores project data in a GeoPackage, a portable SQLite-based GIS database. Current versions described by the manual save the QGIS project into that database and can initialize it with demonstration parameters and data.

:::{container} manual-code-divergence
**Version-sensitive information.** The manual mentions QGIS 3.28 LTR as a suggested environment and describes IdrAgraTools v4 as of February 2025. Treat interface names and compatibility recommendations as historical until checked against the plugin version actually installed. The core source in this repository cannot verify QGIS/plugin requirements.
:::

## Recommended workflow

### 1. Create and inspect the database

Create a new IdrAgraTools database, select the projected CRS, and optionally load the demo dataset. The generated QGIS project organizes layers and tables by purpose, including domain, weather, soil, land use, irrigation network and units, elevation, groundwater, analysis control points, and simulation outputs.

Changing layer styling does not affect model data. Avoid deleting generated layers: the plugin relies on their schemas and may reload required layers when the project is reopened.

### 2. Populate model inputs

The manual offers dedicated import commands for:

- weather-station points and weather time series;
- soil and land-use maps;
- irrigation-unit and irrigation-method maps;
- irrigation-network nodes, links, and discharge series;
- the calculation domain; and
- elevation and dated groundwater rasters.

The import dialogs map fields from an existing QGIS layer or file into the plugin database. Standard QGIS editing and copy/paste also work when geometry types and destination field names/types match. Save edits explicitly after manual changes.

For time series, select the destination variable and station/node, identify time and value columns, specify date format, separator, and skipped rows, then validate the import. Large datasets may be faster through the plugin's bulk-import processing algorithm or a database import.

### 3. Configure the run

The **Set simulation** dialog described in v4 covers:

- irrigation mode and irrigation-season dates;
- first and last simulation years;
- raster cell size and domain extent, or polygon-centroid computing elements;
- evaporative- and transpirative-layer thickness;
- capillary-rise settings and terrain-slope bounds;
- output path; and
- monthly or custom-period output aggregation.

The core additionally accepts exact start/end dates and [scheduled Mode 4](scheduled_mode.md) through `idragra_parameters.txt`; these are documented in the [parameter reference](parameters.md).

### 4. Export in dependency order

The plugin's simulation menu separates export into several steps:

1. **Export meteo data** writes station time series, `weather_stations.dat`, station-weight grids, and inputs needed by crop preprocessing.
2. **Export spatial data** writes the ESRI ASCII grids and `rice_soilparam.txt`.
3. **Export irrigation methods** writes the method list and one parameter file per method.
4. **Export water sources data** writes source/IU links, district rules, monitored series, and runtime-source definitions used by [USE mode](use_mode.md).
5. **Export simulation project** writes `idragra_parameters.txt` and Windows batch launchers.
6. **Run CropCoef** creates the station-specific crop/phenology files.
7. **Run IdrAgra** executes the water-balance and irrigation simulation.

The manual's **Run all** command chains these operations. The current IdrAgra executable reads crop definitions and the annual CO2 table directly, so its own run does not require step 6 or the generated `pheno/` folder. The plugin workflow described here may still invoke CropCoef; update its exported `idragra_parameters.txt` to include `CropInputsFolder`, `CropFolder`, and `CO2Filename` before using this executable.

Whenever database inputs or simulation settings change, re-run every export/preprocessing step affected by that change before launching IdrAgra again.

## Exported names versus core defaults

The v4 manual uses this exported layout:

| IdrAgraTools export | Corresponding parameter |
|---|---|
| `geodata/` | {ref}`InputPath <parameter-inputpath>` |
| `meteodata/` | {ref}`MeteoPath <parameter-meteopath>` |
| `pheno/` | Legacy CropCoef output; current IdrAgra does not read it |
| `irrmethods/` | {ref}`IrrMethPath <parameter-irrmethpath>` |
| `wsources/` | {ref}`WatSourPath <parameter-watsourpath>` |
| `simout/` | {ref}`OutputPath <parameter-outputpath>` |

These are also the names used by the bundled demo. They differ from the executable's internal defaults, so keep the generated path settings with the exported folders. See [Project structure](project_structure.md) for the current core-side file inventory.

There is also a small filename difference in the supplied materials: the v4 manual calls the district file `irr_district.txt`, while the current parameter default and bundled demo use `irr_districts.txt`.

:::{container} manual-code-divergence
**Manual/code divergence to review.** {ref}`IrrDistr_list_fn <parameter-irrdistr-list-fn>` defaults to `irr_districts.txt` (plural), and the demo contains that plural filename. The IdrAgraTools v4 manual lists `irr_district.txt` (singular). A plugin export is safe if its generated `idragra_parameters.txt` names the file it actually produced; otherwise this mismatch will cause Mode 1 to fail while opening water-source inputs.
:::

## Reading run feedback

Export, CropCoef, and IdrAgra messages appear in the plugin's process dialog. Resolve errors at the stage that produced them rather than continuing with stale files. After a successful simulation, IdrAgraTools can generate input-overview and annual/irrigation-unit reports and can import or aggregate output maps for QGIS analysis.

The available report and analysis commands are plugin-version dependent. The core output switches documented in [parameters.md](parameters.md) determine which source files are available for those tools.
