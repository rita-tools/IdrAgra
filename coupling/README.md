# IdrAgra v2 / MODFLOW API prototype

This protoype uses the official MF6 API to couple IdrAgra and MODFLOW simulations.
The two models run indipendently and exchange data every x days (specified in `modflow_parameters.txt`), waiting for each other before continuing. Information exchange is done through .asc files stored in a temporary coupling folder:
- exchange_NNNNNN.asc (cumulative net percolation through the bottom of IdrAgra's profile over the period [mm])
- pumping_NNNNNN.asc (cumulative uptake from irrigation wells over the period [mm])
- water_table_NNNNNN.asc (MODFLOW-updated water table depth [m from soil surface])
MODFLOW receives exchange and pumping, updates the water table, and feeds it back to IdrAgra.

It is assumed that the user has a pre-existing MODFLOW 6 simulation ready to use, which should:
- Have one stress period per coupling period, including the potentially shorter final period
- Use metres as its length unit and seconds as its time unit (the coupler supplies recharge in m/s and pumping in m3/s)
- Use layered DISV discretization with one vertical groundwater column for each entry in IdrAgra's `domain.asc`
- Include an RCH package containing one entry per complete grid cell (setting its values to 0 is ok as they are overridden by the coupling)
- In USE mode (`Mode = 1`), include the WEL package used for IdrAgra pumping. WEL is optional in every other mode.
- Have initial heads representing the same condition as IdrAgra's `waterdepth.asc`

## Configuration

Download the MODFLOW 6.7.0 windows zip package and exctract its libmf6.dll to a known location

Install the Python dependencies numpy and modflowapi:
    python -m pip install "modflowapi>=1.0.1,<2" numpy

In the IdrAgra working directory:
1. Set `UseModflowCoupling = T` in `idragra_parameters.txt` (also make sure that `CapillaryFlag = T`)
2. Copy `modflow_parameters_example.txt` to the working directory as `modflow_parameters.txt` and adjust its values.

## Run

NOTE: due to the stdin channel being needed for the exchange of signals between the models, the coupled run doesn't wait for human input before overwriting output folders. Proceed with caution!

The coupled run can exclusively be launched via the modflow_coupler.py script like so:
    python coupling/modflow_coupler.py --working-dir C:/path/to/idragra_project --idragra-exe C:/path/to/idragra_latest.exe

For this repository layout:
    python coupling/modflow_coupler.py --working-dir demo --idragra-exe release/idragra_latest.exe

Normal IdrAgra command line arguments are still usable by adding them after an empty "--", e.g.:
    python coupling/modflow_coupler.py --working-dir demo --idragra-exe release/idragra_latest.exe -- -verbose -f my_idragra_parameters.txt

## Additional notes

- IdrAgra's warmup year (`InitialThetaFlag = F`) is not coupled. MODFLOW period 1 corresponds to `StartSimulation`; during warmup IdrAgra uses its configured initial water-table grid.
- IdrAgra domain entries and MODFLOW horizontal columns must use the same ordering; the coupler does not perform geographic remapping
- Note that even if MODFLOW were to prescribe a very shallow / above-ground water table, IdrAgra internally enforces a minimum depth equal to its first layer's thickness
- For multilayer models, the water table is taken from the uppermost active, saturated, convertible cell in each vertical column. Inactive and vertical pass-through cells are skipped.
- IdrAgra pumping is passed to MODFLOW only in USE mode (`Mode = 1`). In other modes, existing MODFLOW WEL rates are left unchanged.
- MODFLOW simulates all cells in `domain.asc`, including IdrAgra's nodata cells; those cells currently have both pumping and percolation = 0
