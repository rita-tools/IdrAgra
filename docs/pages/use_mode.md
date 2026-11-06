(mode-use)=
# USE mode: water availability and delivery

:::{container} llm-review-note
**LLM-authored draft — review required.** This page is an LLM-written practical summary of USE mode based on the 2025 manuals and the current IdrAgra inputs. It should be reviewed by a model maintainer.
:::

Select USE mode with {ref}`Mode = 1 <parameter-mode>` when irrigation must be limited by the water available from specified sources. Unlike [NEED modes](need_modes.md), USE mode can leave some crop demand unsatisfied when the available delivery is insufficient.

## Main concepts

**Irrigation unit (IU), or irrigation district**
: An area supplied through the same delivery system. `irr_units.asc` assigns each irrigated cell to an irrigation unit.

**Water source**
: A monitored or estimated supply that can deliver water to one or more irrigation units. Examples include monitored diversions, internal reuse, collective runtime sources, and private wells.

**Conveyance efficiency**
: The fraction of water that reaches an irrigation unit after conveyance and distribution losses. `conv_eff.asc` supplies this value for each cell.

**Irrigation method**
: The field application method assigned through `irr_meth.asc`. It controls application depth, activation thresholds, timing, wetted fraction, interception, and losses; see {ref}`Irrigation methods <irrigation-method-files>`.

## How water reaches cells

For each day of the irrigation season, IdrAgra:

1. reads or estimates the water available from each source;
2. directs the specified share of each source to its connected irrigation units;
3. applies conveyance efficiency;
4. identifies cells whose crops and soil-water conditions allow irrigation; and
5. distributes the available water among those cells.

Water that cannot yet provide a usable application may be carried into the following day. Any remaining water after the unit has been processed is reported as surplus. Private-well irrigation is considered separately for districts that allow it.

## Water-source types

The `SOURCE_TYPE` column in `watsources.txt` accepts:

| Code | Source represented |
|---:|---|
| 1 | first monitored-source series |
| 2 | second monitored-source series |
| 3 | monitored internal-reuse series |
| 4 | collective source estimated from runtime and activation settings |

Each row connects a source to an irrigation district and gives the fraction of that source directed to the district. A source may therefore appear in more than one row.

## Required inputs

Set the following in `idragra_parameters.txt`:

- {ref}`Mode = 1 <parameter-mode>`;
- {ref}`IrrMethPath <parameter-irrmethpath>` and {ref}`IrrMethFileName <parameter-irrmethfilename>`;
- {ref}`WatSourPath <parameter-watsourpath>`;
- {ref}`WatSources_fn <parameter-watsources-fn>` for source-to-district connections; and
- {ref}`IrrDistr_list_fn <parameter-irrdistr-list-fn>` for district settings.

The spatial folder set by {ref}`InputPath <parameter-inputpath>` must contain `irr_meth.asc`, `irr_units.asc`, and `conv_eff.asc`, or the required yearly irrigation-method maps when land use varies. Source-series files are required for the source types declared in `watsources.txt`.

Use {ref}`StartIrrSeason <parameter-startirrseason>` and {ref}`EndIrrSeason <parameter-endirrseason>` as the general irrigation season. Individual irrigation methods may override those dates.

## Relevant outputs

USE mode can write daily tables for irrigation-unit supply, used water, surplus, carried-over water, private supply, and delivery order. Their current filenames and meanings are listed under {ref}`USE-mode and station tables <use-mode-output-tables>`.

For a complete file inventory, see [Project structure](project_structure.md) and [Simulation outputs](outputs.md).
