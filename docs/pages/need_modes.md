(mode-need)=
# NEED modes: calculated irrigation requirements

:::{container} llm-review-note
**LLM-authored draft — review required.** This page is an LLM-written practical summary of NEED modes based on the 2025 manuals and the current IdrAgra inputs. It should be reviewed by a model maintainer.
:::

NEED modes estimate irrigation from crop and soil-water conditions without limiting the result to a measured source supply. Use them to calculate irrigation requirements or to compare management strategies without modelling a delivery network.

## Choosing between Modes 2 and 3

| Setting | Mode 2: field-capacity target | Mode 3: fixed depth |
|---|---|---|
| {ref}`Mode <parameter-mode>` | `2` | `3` |
| Irrigation trigger | soil-water status | soil-water status |
| Application | amount needed to replenish toward field capacity | fixed `Qwat` or `Qadaq` from the irrigation method |
| Application-efficiency map | `appl_eff.asc` required | not used |
| Water-source and district files | not used | not used |

In Mode 2, {ref}`fc_ratio <parameter-fc-ratio>` scales the field-capacity target. A value of `1` uses the full calculated target; a lower value applies a corresponding fraction.

In Mode 3, the irrigation-method file supplies the depth applied whenever the irrigation criterion is met. This is useful for representing a known application depth rather than calculating a refill amount.

## Required inputs

Both NEED modes require:

- {ref}`IrrMethPath <parameter-irrmethpath>` and {ref}`IrrMethFileName <parameter-irrmethfilename>`;
- `irr_meth.asc`, or `irr_meth_<year>.asc` when {ref}`SoilUseVarFlag <parameter-soilusevarflag>` is enabled; and
- valid method IDs for the cells and crops that may be irrigated.

Mode 2 additionally requires `appl_eff.asc`, or `appl_eff_<year>.asc` for yearly land-use maps. Store these grids in {ref}`InputPath <parameter-inputpath>`.

Use {ref}`StartIrrSeason <parameter-startirrseason>` and {ref}`EndIrrSeason <parameter-endirrseason>` as the general irrigation season. Individual methods may provide their own dates.

## Results

The irrigation maps in [Simulation outputs](outputs.md) report the applications calculated by the selected NEED mode. Because source availability is not represented, these results are requirements rather than confirmation that the water could be supplied by a real network.

Return to [Irrigation modes and inputs](irrigation.md) for the full mode comparison.
