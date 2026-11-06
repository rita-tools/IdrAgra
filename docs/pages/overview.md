# Model overview

IdrAgra (standing for *Idrologia Agraria*, Italian for "agricultural hydrology") is a distributed-parameter conceptual model, which allows the simulation of irrigation water distribution in agricultural areas and the estimation of the hydrological balance on a daily basis.

On each simulation day, IdrAgra represents crop development and the soil-crop-atmosphere water balance, accounting for precipitation, irrigation, canopy interception, runoff, evaporation, transpiration, percolation, ponding, and optional capillary rise. These calculations are used to estimate irrigation demand and water distribution according to the selected simulation mode, as well as the effects of water and heat stress on crop yield.

Note that while most of the model's input-output is at the daily scale, IdrAgra internally updates the water balance at an hourly time-step.

## Theoretical framework

The simulation of evapotranspiration and water-stress response are theoretically grounded in FAO's Irrigation and Drainage Paper 56 ([Allen et al., 1998](https://www.fao.org/4/x0490e/x0490e00.htm)), specifically in its [dual crop coefficient approach](<https://www.fao.org/4/x0490e/x0490e0c.htm#chapter%207%20%20%20etc%20%20%20dual%20crop%20coefficient%20(kc%20=%20kcb%20+%20ke)>). Crop development is simulated using growing-degree-day-based phenology.

The discretization of the soil profile into two distinct layers (referred to as the "*evapo-transpirative*"[^evaporative-layer] and "*transpirative*" layers), is also coherent with FAO56. Layers act as stacked "reservoirs", capable of holding an amount of water ranging from residual humidity up to saturation, and exchanging fluxes with each other as well as the soil surface, the water table (if present) and the crop.

After accounting for canopy interception (von Hoyningen-Huene, 1981) and surface runoff ([SCS's Curve Number method](https://www.hec.usace.army.mil/confluence/hmsdocs/hmstrm/canopy-surface-infiltration-and-runoff-volume/infiltration/scs-curve-number-loss-model)), the remaining rainfall may enter the soil profile. From there, water percolates downwards according to each layer's unsaturated hydraulic conductivity, which is calculated using a Brooks-Corey-type equation. Percolation might be boosted around irrigation events to facilitate infiltration, with the booster acting as an empirical proxy for otherwise not simulated phenomena like hydraulic gradients and preferential flow.

If the user provides water table data, the model can additionally calculate capillary uptake through the bottom of the profile using the empirical model developed by [Liu et al. (2006)](https://www.sciencedirect.com/science/article/pii/S0378377406000321).

(simulation-unit)=
## Simulation unit

The domain of an IdrAgra simulation is comprised of **cells** (sometimes referred to as "fields"), which are portions of land considered uniform in terms of weather, soil, landuse and management.

The cell is IdrAgra's smallest independent computational unit: it is at the cell level that the model performs most operations, such as solving the soil-crop-atmosphere balance, applying irrigation water, and calculating yield.\
Each cell is identified by its position in the simulation's domain (row, column) and receives spatialized input data such as soil properties, land use and irrigation method through dedicated .asc files. Other inputs, such as weather and crop phenology series, are provided at the weather station level, but are spatialized to the cell level internally.

While cells are usually squared grid elements, it is possible for IdrAgra to simulate any kind of cell shape. To do so, use the optional `shapearea.asc` file to indicate the area [m<sup>2</sup>] represented by each cell. The easiest way to set up a non grid-based simulation is through [IdrAgraTools](idragratools_workflow), a QGIS plugin that acts as IdrAgra's pre- and post-processor.

## Simulation modes

The way irrigation is accounted for by the model varies considerably according to the simulation mode selected by the user.\
The main distinction is between the so-called "**NEED**" and "**USE**" modes, with the former simulating an ideal scenario in which irrigation events are triggered by RAW depletion, and the latter attempting to simulate a real-world scenario in which irrigation scheduling is turn-based and is constrained by water availability in the distribution network.

IdrAgra provides five choices through {ref}`Mode <parameter-mode>`:

| Mode | Descriptive name | Practical meaning |
|---:|---|---|
| 0 | No irrigation | Run crop and water-balance calculations without irrigation applications |
| 1 | [USE](use_mode.md) | Simulate water movement from sources to irrigation units, then distribute the available volume among eligible cells |
| 2 | [NEED, field-capacity target](need_modes.md) | triggered by RAW depletion; apply amount needed to restore field capacity |
| 3 | [NEED, fixed volume](need_modes.md) | triggered by RAW depletion; apply method-dependent fixed amount |
| 4 | [Scheduled](scheduled_mode.md) | Irrigation depths and dates are read from {ref}`file <parameter-sched-irr-fn>` |


[^evaporative-layer]: Note that the "evapo-transpirative" layer is sometimes referred to simply as the “evaporative" layer because, in earlier versions of the model, it was not affected by transpiration fluxes.