(irrigation-overview)=
# Irrigation modes and inputs

:::{container} llm-review-note
**LLM-authored draft — review required.** This page is an LLM-written practical summary of the 2025 technical manual. Mode selection and input names were checked against the current IdrAgra code; the guidance should still be reviewed by a model maintainer.
:::

Set the irrigation strategy with {ref}`Mode <parameter-mode>` in `idragra_parameters.txt`:

| Mode | Strategy | Use it when... |
|---:|---|---|
| 0 | No irrigation | You want a rainfed simulation with no irrigation applications. |
| 1 | [USE](use_mode.md) | Irrigation is limited by water-source availability and delivery through irrigation units. |
| 2 | [NEED: field-capacity target](need_modes.md) | You want IdrAgra to estimate the depth needed to replenish soil moisture. |
| 3 | [NEED: fixed depth](need_modes.md) | You want soil moisture to trigger a fixed application depth. |
| 4 | [Scheduled irrigation](scheduled_mode.md) | You have dated irrigation instructions for irrigation units. |

Mode 0 requires no irrigation maps or method files. The crop and soil-water balance still run, so it can be used to represent rainfed conditions or compare irrigated and non-irrigated scenarios.

(irrigation-method-files)=
## Irrigation methods

Modes 1–4 use irrigation-method files stored in {ref}`IrrMethPath <parameter-irrmethpath>`. {ref}`IrrMethFileName <parameter-irrmethfilename>` identifies the list of available methods, and `irr_meth.asc` assigns a method ID to each irrigated cell. If {ref}`SoilUseVarFlag <parameter-soilusevarflag>` is enabled, use `irr_meth_<year>.asc` instead.

The list file contains `IrrMethNum`, followed by a `List` block with one filename per method. Each method file can contain:

| Setting | What the user controls |
|---|---|
| `Id` | method ID used in the irrigation-method map |
| `Qwat` or `Qadaq` | fixed application depth in mm |
| `fw` | fraction of the soil surface wetted |
| `K_stress` or `irr_th_mn` | soil-moisture threshold for collective or monitored supply |
| `K_stresswells` or `irr_th_unm` | soil-moisture threshold for private supply |
| `Min_a`, `Max_a`, `Min_b`, `Max_b` | limits for the post-irrigation percolation response |
| `InterceptionFlag` | whether irrigation is applied above the crop canopy |
| `a`, `b`, `c` | application-loss coefficients |
| `irr_starts`, `irr_ends` | optional method-specific irrigation season |
| `h_maxpond` | maximum ponding depth in mm |
| `1` ... `24` | fraction of the daily application assigned to each hour |

If a method does not provide its own season, IdrAgra uses {ref}`StartIrrSeason <parameter-startirrseason>` and {ref}`EndIrrSeason <parameter-endirrseason>`. See [Flooded rice](rice.md) for the additional role of ponding depth in paddy fields.

## Inputs by mode

| Input | 0 | [1](use_mode.md) | [2](need_modes.md) | [3](need_modes.md) | [4](scheduled_mode.md) |
|---|:---:|:---:|:---:|:---:|:---:|
| Irrigation-method list and files | — | ✓ | ✓ | ✓ | ✓ |
| `irr_meth.asc` or yearly variants | — | ✓ | ✓ | ✓ | ✓ |
| `irr_units.asc` | — | ✓ | — | — | ✓ |
| `conv_eff.asc` | — | ✓ | — | — | ✓ |
| `appl_eff.asc` or yearly variants | — | — | ✓ | — | ✓ |
| Water-source and district files | — | ✓ | — | — | — |
| Scheduled-irrigation file | — | — | — | — | ✓ |

The detailed mode pages explain what each input represents. See the {ref}`spatial-file inventory <spatial-ascii-grids>` for the complete list.
