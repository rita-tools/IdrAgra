# Flooded rice

IdrAgra represents the flooded conditions, ponding, and different soil properties used for paddy rice.

:::{container} llm-review-note
**LLM-authored draft — review required.** This page is an LLM-written practical summary of the supplied 2025 technical manual, checked against current IdrAgra behavior. Paddy inputs and interpretation should be reviewed by a model maintainer.
:::

## Identifying paddy rice

Paddy behavior applies to {ref}`cells <simulation-unit>` where:

- `CL_CN = 7` identifies the crop as rice in its crop parameter `.tab` file;
- `Irrigation = 1` allows the crop to be irrigated; and
- `irr_meth.asc`, or its yearly variant, assigns a valid irrigation method.

IdrAgra does not identify rice from the crop name, land-use label, or the presence of standing water alone.

## Paddy soil parameters

Place the optional `rice_soilparam.txt` file in {ref}`InputPath <parameter-inputpath>`. It accepts:

```text
Ksat_II = <real>
N_II = <real>
ThetaII_FC = <real>
ThetaII_r = <real>
ThetaII_sat = <real>
ThetaII_WP = <real>
```

These values replace the ordinary Layer II properties for irrigated rice cells during the configured irrigation period. Modes 1--4 use the period of the cell's irrigation method; mode 0 uses the global `StartIrrSeason` and `EndIrrSeason` values.
If the file is absent, IdrAgra uses the Layer II soil-property maps instead.

## Flooding and ponding

Paddy irrigation first establishes flooded and saturated conditions. During the flooded period it replaces evapotranspiration and percolation losses, while effective rainfall reduces the required irrigation.

The active {ref}`irrigation method <irrigation-method-files>` supplies the maximum ponding depth during its irrigation season. Outside that season, IdrAgra uses the general {ref}`h_maxpond <parameter-h-maxpond>` value. Water above the permitted ponding depth can become runoff.

:::{container} manual-code-divergence
**Behavior difference to review.** Paddy irrigation in [Mode 2](need_modes.md) is currently calculated differently from Modes 1, 3, and 4. This may produce different paddy-water requirements and needs maintainer review; no model behavior has been changed here.
:::

## Input checklist

Before running an irrigated rice scenario, check that:

1. The rice crop's `.tab` file contains `CL_CN = 7`, `Irrigation = 1`.
2. The irrigation-method map assigns a valid positive method.
3. The method's season, application depth, hourly distribution, and maximum ponding depth represent the intended practice.
4. `rice_soilparam.txt` contains reviewed Layer II values when paddy-specific soil properties are required.

See [Irrigation modes and inputs](irrigation.md) for mode selection and common method settings.
