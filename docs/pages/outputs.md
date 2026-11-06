# Simulation outputs

:::{container} llm-review-note
**LLM-authored draft — review required.** This practical output inventory was drafted from the 2025 installation/use manual and checked against the current output-writing code. Scientific interpretation and units should be reviewed before release.
:::

IdrAgra writes results to {ref}`OutputPath <parameter-outputpath>`. Which files are created is controlled mainly by the {ref}`output switches <parameter-output-controls>` in `idragra_parameters.txt`.

## Periodic map names

{ref}`MonthlyFlag <parameter-monthlyflag>` selects monthly, weekly, or custom-period aggregation. The filename prefix is:

```text
<year>_month_<number>_   # monthly
<year>_week_<number>_    # weekly
<year>_step_<number>_    # custom period
```

The current standard suffixes are:

| Suffix | Quantity |
|---|---|
| `prec.asc` | precipitation |
| `trasp_act.asc`, `trasp_pot.asc` | actual and potential transpiration |
| `irr.asc`, `irr_loss.asc` | gross irrigation and application bypass/loss |
| `caprise.asc` | capillary rise |
| `irr_privw.asc`, `irr_distr.asc` | private-well and unmonitored collective irrigation components |
| `flux2.asc` | deep percolation from Layer II |
| `runoff.asc` | runoff |
| `et_pot.asc`, `et_act.asc` | potential and actual evapotranspiration |

Most water-depth maps are accumulated in millimetres over the output period. `theta1.asc` and `theta2.asc`, when enabled as debug maps, are end-of-period soil-water states rather than sums.

Additional periodic debug suffixes are `eva.asc`, `prec_eff.asc`, `perc1.asc`, `perc2.asc`, `theta1.asc`, and `theta2.asc`. They are controlled by their own `prt_dbg_*` switches.

## Annual water-balance maps

Annual files begin with `<year>_`:

| Suffix | Quantity |
|---|---|
| `prec_tot.asc` | total precipitation |
| `prec_agr.asc` | precipitation during crop periods |
| `irr_tot.asc`, `irr_loss.asc` | gross irrigation and application bypass/loss |
| `eva_act_agr.asc`, `eva_pot_agr.asc` | actual and potential soil evaporation during crop periods |
| `trasp_act_tot.asc`, `trasp_pot_tot.asc` | actual and potential transpiration |
| `run_tot.asc` | runoff |
| `net_flux_gw.asc` | Layer II deep percolation minus capillary rise |
| `eff_tot.asc` | seasonal water-use efficiency ratio |
| `irr_nr.asc`, `irr_mean.asc` | irrigation-event count and mean event depth |

The yearly debug maps are `eva_tot.asc`, `eff_prec_tot.asc`, `iter1.asc`, and `iter2.asc` when their corresponding switches are enabled.

:::{container} manual-code-divergence
**Manual/code divergence to review.** The installation manual calls the annual groundwater-flux result `flux_tot.asc`. The file produced by IdrAgra is `net_flux_gw.asc`, representing deep percolation minus capillary rise.
:::

## Yield and stress maps

Yield-related maps add crop or stage indices to their roots. Current examples include:

```text
<year>_biomass_pot_<crop>.asc
<year>_yield_pot_<crop>.asc
<year>_yield_act_<crop>.asc
<year>_T_act_sum_<stage>_<crop>.asc
<year>_T_pot_sum_<stage>_<crop>.asc
<year>_fcCS_<stage>_<crop>.asc
<year>_fcT_<crop>.asc
<year>_fHS_<crop>.asc
<year>_fHS_sum_<crop>.asc
```

The crop and stage numbers correspond to the crop sequence and phenological stages in the crop input files; they are not necessarily the land-use ID shown on a map.

(use-mode-output-tables)=
## USE-mode and station tables

[USE mode](use_mode.md) can create semicolon-delimited annual daily tables using the following filename endings:

| Ending | Contents |
|---|---|
| `_Qirrunits.csv` | irrigation-unit supply and demand information |
| `_Qcrs.csv` | collective runtime-source information, when present |
| `_Qirr.csv` | source withdrawals or deliveries used by the model |
| `_Qsurplus.csv` | source surplus |
| `_Qrem.csv` | water retained for delivery on a later day |
| `_Qprivate.csv` | private-source irrigation |
| `_Watshift.csv` | irrigation-unit delivery-order state |
| `_et0_stations.csv` | station reference evapotranspiration when `prt_cell_et0 = y` |

Exact columns depend on the active source types and irrigation units. These tables should be treated as model diagnostics as well as results; inspect their headers rather than assuming a fixed column count.

(selected-cell-output-tables)=
## Selected-cell tables

When `cells.txt` is present in the working directory, its selections produce annual files such as:

```text
<year>_cell_<row>_<col>.csv
<year>_cellinfo_<row>_<col>.csv
<year>_cellparameters_<row>_<col>.csv
```

The main `cell` table is a daily time series. The `cellinfo` and `cellparameters` tables record the selected cell's input identifiers and parameters. Optional detailed files use `_convergence_`, `_cellevaporation_`, and `_cellrunoff_` in their names.

:::{container} manual-code-divergence
**Manual/code divergence to review.** The manual associates `-v` with producing all or debug outputs. In IdrAgra, `-v` only adds console messages; use the {ref}`output switches <parameter-output-controls>` to select files, or `-s` for the reduced irrigation-map summary.
:::
