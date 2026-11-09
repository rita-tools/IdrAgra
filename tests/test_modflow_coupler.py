import unittest
from types import SimpleNamespace

import numpy as np

from coupling.modflow_coupler import (
    CouplerError,
    GridLayout,
    _heads_to_water_table_depth,
    _validate_modflow_grid,
)


class Array:
    def __init__(self, values):
        self.values = values


def package(package_type, **arrays):
    return SimpleNamespace(
        pkg_type=package_type,
        **{name: Array(values) for name, values in arrays.items()},
    )


def layered_model():
    disv = package(
        "disv",
        top=[10.0, 10.0, 10.0],
        botm=[[5.0, 5.0, 5.0], [0.0, 0.0, 0.0]],
        idomain=[[1, 1, -1], [1, 1, 1]],
        area=[100.0, 100.0, 100.0],
    )
    npf = package("npf", icelltype=[[1, 1, 1], [1, 1, 1]])
    return SimpleNamespace(shape=(2, 3), package_dict={"disv": disv, "npf": npf})


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

    def test_rejects_non_disv_discretization(self):
        model = layered_model()
        model.package_dict["disv"].pkg_type = "dis"

        with self.assertRaisesRegex(CouplerError, "no DISV package"):
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
        model.package_dict["disv"].idomain.values = [[1, 0, -1], [1, 0, 1]]
        grid = _validate_modflow_grid(model, self.layout)

        with self.assertRaisesRegex(CouplerError, "1 MODFLOW column"):
            _heads_to_water_table_depth([[8.0, 4.0, 8.0], [3.0, -1.0, 2.0]], grid, self.layout.shape)


if __name__ == "__main__":
    unittest.main()
