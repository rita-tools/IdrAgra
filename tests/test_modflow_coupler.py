import unittest
from io import StringIO
from pathlib import Path
from tempfile import TemporaryDirectory
from types import SimpleNamespace

import numpy as np

from coupling.modflow_coupler import (
    AsciiGrid,
    CouplerError,
    GridLayout,
    IdrAgraProcess,
    ModflowConfiguration,
    _heads_to_water_table_depth,
    _is_USE_mode,
    _validate_modflow_grid,
)


class AsciiGridTests(unittest.TestCase):
    def test_preserves_single_column_shape(self):
        contents = """ncols 1
nrows 3
xllcorner -1
yllcorner -1
cellsize 10
NODATA_value -9999
1
2
3
"""
        with TemporaryDirectory() as directory:
            path = Path(directory) / "domain.asc"
            path.write_text(contents, encoding="utf-8")

            grid = AsciiGrid.read(path)

        self.assertEqual(grid.data.shape, (3, 1))
        np.testing.assert_array_equal(grid.data, [[1.0], [2.0], [3.0]])


class ModflowConfigurationTests(unittest.TestCase):
    def test_start_date_is_optional(self):
        with TemporaryDirectory() as directory:
            path = Path(directory) / "modflow_parameters.txt"
            path.write_text(
                "LibMf6 = libmf6.dll\nModflowWorkspace = model\nCouplingPeriodDays = 7\n",
                encoding="utf-8",
            )

            configuration = ModflowConfiguration.read(path)

        self.assertIsNone(configuration.start_date)

    def test_reads_and_normalizes_start_date(self):
        with TemporaryDirectory() as directory:
            path = Path(directory) / "modflow_parameters.txt"
            path.write_text(
                "LibMf6 = libmf6.dll\nModflowWorkspace = model\n"
                "CouplingPeriodDays = 7\nModflowStartDate = 9/2/2023\n",
                encoding="utf-8",
            )

            configuration = ModflowConfiguration.read(path)

        self.assertEqual(configuration.start_date, "09/02/2023")

    def test_rejects_invalid_start_date(self):
        with TemporaryDirectory() as directory:
            path = Path(directory) / "modflow_parameters.txt"
            path.write_text(
                "LibMf6 = libmf6.dll\nModflowWorkspace = model\n"
                "CouplingPeriodDays = 7\nModflowStartDate = 31/2/2023\n",
                encoding="utf-8",
            )

            with self.assertRaisesRegex(CouplerError, "dd/mm/yyyy"):
                ModflowConfiguration.read(path)


class FinishedModflowTests(unittest.TestCase):
    def test_finishes_coupling_at_next_idragra_handshake(self):
        with TemporaryDirectory() as directory:
            idragra = IdrAgraProcess.__new__(IdrAgraProcess)
            idragra.coupling_dir = Path(directory)
            idragra.process = SimpleNamespace(
                stdout=["IDRAGRA_COUPLING_READY 109 7\n", "IdrAgra finished\n"],
                stdin=StringIO(),
                wait=lambda: 0,
            )

            idragra.finish_simulation()

            self.assertEqual(idragra.process.stdin.getvalue(), "IDRAGRA_COUPLING_FINISH 109\n")
            self.assertEqual(list(Path(directory).glob("water_table_*.asc")), [])


class CouplingModeTests(unittest.TestCase):
    def test_reads_use_mode(self):
        with TemporaryDirectory() as directory:
            path = Path(directory) / "idragra_parameters.txt"
            path.write_text("Mode = 1\n", encoding="utf-8")

            mode = _is_USE_mode(path)

        self.assertEqual(mode, 1)

class Array:
    def __init__(self, values):
        self.values = values


def package(package_type, **arrays):
    return SimpleNamespace(
        pkg_type=package_type,
        **{name: Array(values) for name, values in arrays.items()},
    )


def layered_model():
    dis = package(
        "dis",
        top=[[10.0, 10.0, 10.0], [5.0, 5.0, 5.0]],
        bot=[[5.0, 5.0, 5.0], [0.0, 0.0, 0.0]],
        idomain=[[1, 1, -1], [1, 1, 1]],
        area=[[100.0, 100.0, 100.0], [100.0, 100.0, 100.0]],
    )
    npf = package("npf", icelltype=[[1, 1, 1], [1, 1, 1]])
    return SimpleNamespace(shape=(2, 3), package_dict={"dis": dis, "npf": npf})


class ModflowGridTests(unittest.TestCase):
    def setUp(self):
        self.layout = GridLayout([], (3, 1), 1.0)

    def test_accepts_multiple_layers_with_one_column_per_idragra_cell(self):
        grid = _validate_modflow_grid(layered_model(), self.layout)

        self.assertEqual(grid.nlay, 2)
        self.assertEqual(grid.ncolumns, 3)
        np.testing.assert_array_equal(grid.idomain, [[1, 1, -1], [1, 1, 1]])

    def test_rejects_horizontal_column_count_mismatch(self):
        with self.assertRaisesRegex(CouplerError, "horizontal columns"):
            _validate_modflow_grid(layered_model(), GridLayout([], (4, 1), 1.0))

    def test_rejects_structured_discretization(self):
        model = layered_model()
        model.shape = (2, 1, 3)

        with self.assertRaisesRegex(CouplerError, "expected layered DISV"):
            _validate_modflow_grid(model, self.layout)

    def test_selects_uppermost_active_saturated_convertible_head(self):
        grid = _validate_modflow_grid(layered_model(), self.layout)
        heads = [[8.0, 4.0, 8.0], [3.0, 3.0, 2.0]]

        depth = _heads_to_water_table_depth(heads, grid, self.layout.shape)

        np.testing.assert_allclose(depth, [[2.0], [7.0], [8.0]])

    def test_uses_deepest_active_bottom_for_a_dry_column(self):
        grid = _validate_modflow_grid(layered_model(), self.layout)
        heads = [[8.0, 4.0, 8.0], [3.0, -1.0, 2.0]]

        depth = _heads_to_water_table_depth(heads, grid, self.layout.shape)

        np.testing.assert_allclose(depth, [[2.0], [10.0], [8.0]])

    def test_rejects_column_without_an_active_cell(self):
        model = layered_model()
        model.package_dict["dis"].idomain.values = [[1, 0, -1], [1, 0, 1]]
        grid = _validate_modflow_grid(model, self.layout)

        with self.assertRaisesRegex(CouplerError, "1 MODFLOW column"):
            _heads_to_water_table_depth([[8.0, 4.0, 8.0], [3.0, -1.0, 2.0]], grid, self.layout.shape)


if __name__ == "__main__":
    unittest.main()
