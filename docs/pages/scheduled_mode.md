(mode-scheduled)=
# Scheduled irrigation mode

:::{container} llm-review-note
**LLM-authored draft — review required.** This page is an LLM-written guide to scheduled Mode 4 based on the current IdrAgra inputs. This mode is not covered in detail by the supplied 2025 manuals and should be reviewed by a model maintainer.
:::

Select scheduled irrigation with {ref}`Mode = 4 <parameter-mode>` when irrigation dates and amounts are known in advance. Each schedule row applies to an irrigation unit, so cells must be grouped with `irr_units.asc`.

## Schedule file

The file selected by {ref}`sched_irr_fn <parameter-sched-irr-fn>` is stored in {ref}`WatSourPath <parameter-watsourpath>`. Its first line is the number of records, its second line is a header, and the remaining rows contain:

```text
irrigation_unit_id  year  day_of_year  water_depth_mm
```

For example:

```text
3
IU  Year  DoY  Depth
1   2022  120  25
1   2022  135  -1
2   2022  140  -10
```

The depth field accepts:

| Value | Result |
|---:|---|
| positive number | apply that depth in mm |
| `-1` | use the fixed depth from the cell's irrigation method |
| `-10` | calculate the depth needed to replenish toward field capacity |
| any other non-positive value | no irrigation application |

## Required inputs

Scheduled mode requires:

- {ref}`Mode = 4 <parameter-mode>` and a valid schedule file;
- {ref}`IrrMethPath <parameter-irrmethpath>` and {ref}`IrrMethFileName <parameter-irrmethfilename>`;
- `irr_meth.asc`, `irr_units.asc`, `conv_eff.asc`, and `appl_eff.asc` in {ref}`InputPath <parameter-inputpath>`; and
- valid irrigation-method and irrigation-unit IDs for scheduled cells.

Yearly `irr_meth_<year>.asc` and `appl_eff_<year>.asc` files are used when {ref}`SoilUseVarFlag <parameter-soilusevarflag>` is enabled. Irrigation-method timing, season, canopy interception, and application-loss settings still apply to scheduled events.

:::{container} manual-code-divergence
**Documentation gap to review.** Scheduled Mode 4 is available in the current model but is not described as a selectable mode in the supplied 2025 manuals. Verify a new schedule with a short test simulation before using it for production results.
:::

See [Simulation outputs](outputs.md) for the maps and tables produced by the run, or return to [Irrigation modes and inputs](irrigation.md).
