# Crop yield

IdrAgra estimates biomass and yield for each harvested crop in each cell. Its
yield calculation follows the five parts described in Section 5 of the IdrAgra
technical manual: biomass, water stress over the whole crop and by development
stage, heat stress, potential yield, and actual yield. This page describes the
current executable and points out where it differs from the manual.

:::{container} llm-review-note
***LLM-authored draft — review required.** This page is an LLM rewrite of the manual passage on
crop yield, with checks against the current code.
The heat-sensitive period and the assignment of yield-development stages need
further review.
:::

## Crop occurrence and biomass

The model accumulates yield inputs for the crop currently growing in each cell.
Those sums survive a calendar-year boundary and the warm-up handoff. At harvest,
the model writes the completed result into a slot for that calendar year's yield
maps. An unfinished crop has no completed yield result.

The manual describes biomass as adjusted water productivity multiplied by the
sum of **actual** crop transpiration divided by reference evapotranspiration.
The current code instead calculates potential biomass from **potential** crop
transpiration:

$$
B_{\mathrm{pot}} = WP^{*}_{\mathrm{adj}}
    \sum_{t \in \mathrm{crop}} \frac{T_{\mathrm{pot},t}}{ET_{0,t}}.
$$

The sum includes days with positive $ET_0$. IdrAgra calculates
$WP^{*}_{\mathrm{adj}}$ from the crop's water productivity, sink strength, and
the CO2 concentration of the **harvest year**. CropCoef no longer needs to
provide `WPadj.dat` for this calculation.

:::{container} manual-code-divergence
**Manual/code divergence to review.** Equation 5.1 in the manual uses actual
transpiration for biomass. The executable uses potential transpiration to
estimate `biomass_pot`, then applies water-stress factors to potential yield.
This choice was already present in IdrAgra before the daily CropCoef integration.
:::

## Water-stress reduction

Each day, the model accumulates actual and potential crop transpiration over
the entire crop occurrence and separately for four yield-development stages.
The whole-crop factor is

$$
f_{\mathrm{WS,total}} = \max\!\left(0,
    1-k_{y,0}\left(1-\frac{\sum T_{\mathrm{act}}}
                              {\sum T_{\mathrm{pot}}}\right)\right).
$$

For stage $j$, it forms an analogous factor using that stage's $k_{y,j}$ and
transpiration sums. The four stage factors are multiplied, each raised to the
fraction of counted stage days spent in that stage. The model uses the smaller
of this combined stage factor and the whole-crop factor. A stage without days
or potential transpiration is skipped; if the whole crop has no potential
transpiration, its whole-crop factor is one.

The stage labels are 1 initial, 2 development, 3 mid-season, and 4 late season.
Crops in pre-emergence (sttage 0) are excluded from the
four stage-specific sums and their duration weights. The initial stage ends at the last consecutive point with
the first positive Kcb value; mid-season spans the curve's maximum-Kcb points.

:::{container} manual-code-divergence
**Stage assignment to review.** Crop files do not specify stage boundaries
explicitly. A curve without an initial plateau leaves stage 1 as soon as GDD
passes its first positive Kcb point. A curve with only one maximum-Kcb point
can have no full day in stage 3. These interpretations need scientific review.
:::

## Heat-stress reduction

The manual defines the thermal-sensitive period as the days from 45% through
75% of growing-period length (GPL). Before CropCoef's integration, IdrAgra applied those
fractions to the crop's predicted duration **in days**. The current daily
calculation instead includes a day when accumulated GDD is at least 45% and
less than 75% of the target GDD. The heat factor is the mean of the daily
factors over the included days:

$$
f_{\mathrm{HS},t} =
\begin{cases}
1, & T_t < T_{\mathrm{crit}}, \\
1-\dfrac{T_t-T_{\mathrm{crit}}}{T_{\mathrm{lim}}-T_{\mathrm{crit}}},
   & T_{\mathrm{crit}} \le T_t < T_{\mathrm{lim}}, \\
0, & T_t \ge T_{\mathrm{lim}}.
\end{cases}
$$

Here $T_t$ is the model's daytime-temperature estimate for the cell. If no
days fall in the GDD window, the current code assigns $f_{\mathrm{HS}}=1$.
The crop's heat-stress sum and mean factor are available as optional output
maps.

:::{container} manual-code-divergence
**Thermal-sensitive period to review.** A fraction of target GDD is not the
same as a fraction of GPL in calendar days. It can change both the number of
included days and which weather events affect yield. The GDD-based window
follows a crop across December 31; its suitability and the 45% and 75%
thresholds still need scientific review. It does not imply fewer sensitive
days for every summer crop.
:::

## Potential and actual yield

Potential yield is potential biomass multiplied by the crop's harvest index:

$$Y_{\mathrm{pot}} = HI_0 B_{\mathrm{pot}}.$$

The executable then applies both the water-stress factor and the heat-stress
factor:

$$Y_{\mathrm{act}} = Y_{\mathrm{pot}}
    \min(f_{\mathrm{WS,total}},f_{\mathrm{WS,stage}})
    f_{\mathrm{HS}}.$$

:::{container} manual-code-divergence
**Manual/code divergence to review.** Manual equation 5.8 instead writes
$Y_{\mathrm{act}}=Y_{\mathrm{pot}}\min(f_C,f_{\mathrm{HS}})$.
IdrAgra multiplies its water and heat factors; this also predates the daily
CropCoef integration. The two expressions generally produce different yields.
:::

Enable the yield output switches in the [parameter reference](parameters.md)
to save potential biomass, potential and actual yield, the stress factors,
and stage transpiration sums. See [Simulation outputs](outputs.md) for filenames.
