# Create a planning problem input object

Create a `Problem` object from tabular or spatial inputs.

This is the main entry point to the `mosap` workflow. The function
standardizes planning-unit data, feature definitions, and feature
distributions into the internal structure required by downstream
functions such as
[`add_actions()`](https://josesalgr.github.io/mosap/reference/add_actions.md),
[`add_effects()`](https://josesalgr.github.io/mosap/reference/add_effects.md),
[`add_targets_absolute()`](https://josesalgr.github.io/mosap/reference/add_targets_absolute.md),
[`set_solver()`](https://josesalgr.github.io/mosap/reference/set_solver.md),
and [`solve()`](https://josesalgr.github.io/mosap/reference/solve.md).

Depending on the input types, `inputData()` dispatches to one of several
supported workflows. In all cases, the result is a `Problem` object with
a tabular internal core, optionally augmented with spatial metadata such
as coordinates, geometry, raster references, or raw planning-unit
attributes.

## Usage

``` r
inputData(
  pu,
  features,
  dist_features,
  cost = NULL,
  pu_id_col = "id",
  cost_aggregation = c("mean", "sum"),
  ...
)

# S4 method for class 'data.frame,data.frame,data.frame'
inputData(
  pu,
  features,
  dist_features,
  cost = NULL,
  pu_id_col = "id",
  cost_aggregation = c("mean", "sum"),
  ...
)

# S4 method for class 'ANY,ANY,missing'
inputData(
  pu,
  features,
  dist_features,
  cost = NULL,
  pu_id_col = "id",
  cost_aggregation = c("mean", "sum"),
  ...
)

# S4 method for class 'ANY,ANY,NULL'
inputData(
  pu,
  features,
  dist_features,
  cost = NULL,
  pu_id_col = "id",
  cost_aggregation = c("mean", "sum"),
  ...
)

# S4 method for class 'ANY,data.frame,data.frame'
inputData(
  pu,
  features,
  dist_features,
  cost = NULL,
  pu_id_col = "id",
  cost_aggregation = c("mean", "sum"),
  ...
)
```

## Arguments

- pu:

  Planning-units input. Depending on the selected method, this may be:

  - a `data.frame` with planning-unit information,

  - an `sf` object,

  - a
    [`terra::SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html),

  - a vector file path readable by terra,

  - a
    [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),

  - or a raster file path.

- features:

  Feature input. Depending on the selected method, this may be:

  - a `data.frame` with at least an `id` column,

  - a
    [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
    with one layer per feature,

  - or a raster file path.

- dist_features:

  Feature-distribution input. In tabular and hybrid modes, this must be
  a `data.frame` with columns `pu`, `feature`, and `amount`. In spatial
  modes it may be omitted or set to `NULL`, in which case it is derived
  automatically.

- cost:

  In spatial modes, planning-unit cost information. Depending on the
  input mode, this may be:

  - a column name in the planning-unit attribute table,

  - a raster object,

  - or a raster file path.

  In raster-cell mode, `cost` is required and valid planning units are
  defined only where cost is finite and strictly positive.

- pu_id_col:

  Character string giving the name of the planning-unit id column in
  vector or `sf` inputs. Ignored in purely tabular mode and in
  raster-cell mode.

- cost_aggregation:

  Character string used in vector-PU mode when `cost` is a raster and
  must be aggregated over polygons. Must be one of `"mean"` or `"sum"`.

- ...:

  Additional arguments forwarded to internal builders.

## Value

A `Problem` object to be used in downstream `mosap` workflows. The
object always contains a tabular internal planning core and may
additionally contain spatial metadata depending on the input mode.

## Details

**Overview**

The role of `inputData()` is to construct the internal input data model
used by `mosap`. Regardless of the input mode, the function aims to
produce a consistent problem representation containing, at minimum:

- a planning-unit table `x$data$pu`,

- a feature table `x$data$features`,

- a feature-distribution table `x$data$dist_features`.

Internally, this means that the problem is always reduced to a tabular
core of the form:

- planning units \\i \in \mathcal{P}\\,

- features \\f \in \mathcal{F}\\,

- amounts \\a\_{if} \ge 0\\ stored in `dist_features`,

- and, when available, spatial metadata associated with planning units.

Thus, after `inputData()` has run, the landscape is represented as a set
of planning units and their baseline feature amounts: \$\$ a\_{if} =
\text{amount of feature } f \text{ in planning unit } i. \$\$

These baseline amounts are later combined with actions, effects,
targets, objectives, and constraints to build the optimization model.

**Supported input modes**

Four workflows are supported.

**1. Tabular mode**

If `pu`, `features`, and `dist_features` are all `data.frame` objects,
the problem is built directly from tabular data. No spatial package is
required.

This mode is appropriate when the user already has a tabular
planning-unit table, a feature catalog, and a planning unit–feature
distribution table.

**2. Vector-PU spatial mode**

If `dist_features` is missing or `NULL`, `pu` is a spatial vector
object, and `features` is a raster stack, the function derives
`dist_features` by aggregating raster values over planning-unit
polygons.

In this mode:

- each polygon becomes one planning unit,

- each raster layer becomes one feature,

- feature amounts are aggregated by polygon, currently using `sum`,

- planning-unit cost is taken either from a vector attribute column or
  from a cost raster aggregated over polygons using `cost_aggregation`.

**3. Raster-cell fast mode**

If `dist_features` is missing or `NULL`, and both `pu` and `features`
are rasters, the function enters a raster-cell workflow in which each
valid raster cell becomes one planning unit.

In this mode:

- `pu` is used as a mask or template,

- `cost` must be a single-layer raster,

- a raster cell becomes a planning unit if and only if: \$\$
  \text{\code{pu} is not NA} \quad \text{and} \quad \text{\code{cost} is
  finite and } \> 0, \$\$

- each valid cell is assigned a new sequential planning-unit id,

- feature values are extracted directly by cell without
  raster-to-polygon conversion,

- feature values equal to `NA` are treated as zero before building
  `dist_features`,

- only strictly positive feature amounts are stored in
  `x$data$dist_features`.

This mode is substantially faster than converting large rasters to
polygons and is generally the preferred option for large regular grids.

**4. Hybrid sf-tabular mode**

If `pu` is an `sf` object, while `features` and `dist_features` are
`data.frame`s, the problem is built from the supplied tabular feature
distribution while preserving planning-unit geometry and raw spatial
attributes.

This mode is useful when the user already has a curated tabular
`dist_features` table but still wants to retain geometry for later
spatial operations such as boundary construction, plotting, or spatial
feasibility specifications.

**What is stored in the resulting object**

In all modes, the returned object contains a tabular planning core.
Depending on the input mode, additional components may also be stored:

- `x$data$pu`:

  Planning-unit table, always present.

- `x$data$features`:

  Feature table, always present.

- `x$data$dist_features`:

  Baseline planning unit–feature amounts, always present.

- `x$data$pu_data_raw`:

  Raw planning-unit attributes aligned by planning-unit id, when
  available.

- `x$data$pu_coords`:

  Planning-unit coordinates, when available or derivable.

- `x$data$pu_sf`:

  Planning-unit geometry as `sf`, when available and safely alignable.

- `x$data$pu_raster_id`:

  Raster mask/template used to derive planning units from raster cells
  or rasterized planning units, when available.

- `x$data$cell_index`:

  Indices of valid raster cells in raster-cell mode.

- `x$data$cost_raster`:

  Stored cost raster in raster-cell mode.

- `x$data$features_raster`:

  Stored feature raster stack in raster-cell mode.

**Spatial relations are not created automatically**

This function does *not* create boundary, adjacency, distance, or other
spatial relations automatically. Spatial relations must be added
explicitly afterwards using functions such as:

- [`add_spatial_boundary`](https://josesalgr.github.io/mosap/reference/add_spatial_boundary.md),

- [`add_spatial_rook`](https://josesalgr.github.io/mosap/reference/add_spatial_rook.md),

- [`add_spatial_queen`](https://josesalgr.github.io/mosap/reference/add_spatial_queen.md),

- [`add_spatial_knn`](https://josesalgr.github.io/mosap/reference/add_spatial_knn.md),

- [`add_spatial_distance`](https://josesalgr.github.io/mosap/reference/add_spatial_distance.md),

- [`add_spatial_relations`](https://josesalgr.github.io/mosap/reference/add_spatial_relations.md).

In raster-cell mode, spatial relations will typically be created from
coordinates rather than polygons, for example using
[`add_spatial_knn`](https://josesalgr.github.io/mosap/reference/add_spatial_knn.md)
or
[`add_spatial_distance`](https://josesalgr.github.io/mosap/reference/add_spatial_distance.md).

**Locks are not interpreted automatically**

This function does not interpret planning-unit lock information, even if
the input data contain fields such as `locked_in`, `locked_out`, or
Marxan-style status codes. Such information may be preserved in
`x$data$pu_data_raw`, but it is not activated automatically.

Locked planning units should be defined later with
[`add_locked_pu`](https://josesalgr.github.io/mosap/reference/add_locked_pu.md).

**Feature identifiers**

Features are internally standardized to an `id`-based representation. In
spatial modes where raster layers are used, one feature is created per
raster layer, with:

- `id = 1, 2, ..., nlyr(features)`,

- `name` equal to the raster layer name, if available,

- otherwise a generated name of the form `"feature.1"`, `"feature.2"`,
  and so on.

**Planning-unit identifiers**

In vector and hybrid modes, planning-unit ids are taken from the column
named by `pu_id_col`. If that column is missing and `pu_id_col = "id"`,
sequential ids are created with a warning.

In raster-cell mode, planning-unit ids are always created sequentially
from the valid raster cells retained after masking and cost filtering.

**Cost handling**

In purely tabular mode, `cost` is not used by this generic method
because planning-unit costs are expected to already be present in the
`pu` table supplied to the internal tabular builder.

In spatial modes, `cost` is required:

- in vector-PU mode, it may be either:

  - the name of a numeric attribute column in the planning-unit layer,

  - or a cost raster to be aggregated over polygons;

- in raster-cell mode, it must be a single-layer raster aligned with
  `pu` and `features`;

- in hybrid `sf` + tabular mode, it must be either:

  - the name of an attribute column in the `sf` object,

  - or omitted if the `sf` attributes already contain a column literally
    named `cost`.

**Ordering and alignment**

In spatial modes, planning units, derived coordinates, raw attributes,
geometry, and extracted feature amounts are aligned to the same
planning-unit id order before the final `Problem` object is created.

This alignment is critical because later functions assume that all
stored planning-unit components refer to the same ordered set of
planning units.

## See also

[`add_actions`](https://josesalgr.github.io/mosap/reference/add_actions.md),
[`add_locked_pu`](https://josesalgr.github.io/mosap/reference/add_locked_pu.md),
[`add_spatial_boundary`](https://josesalgr.github.io/mosap/reference/add_spatial_boundary.md),
[`add_spatial_knn`](https://josesalgr.github.io/mosap/reference/add_spatial_knn.md),
[`solve`](https://josesalgr.github.io/mosap/reference/solve.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# ------------------------------------------------------
# 1) Tabular mode
# ------------------------------------------------------
pu <- data.frame(id = 1:3, cost = c(1, 2, 3))
features <- data.frame(id = 1:2, name = c("sp1", "sp2"))
dist_features <- data.frame(
  pu = c(1, 1, 2, 3),
  feature = c(1, 2, 2, 1),
  amount = c(1, 5, 2, 1)
)

x <- inputData(
  pu = pu,
  features = features,
  dist_features = dist_features
)

# ------------------------------------------------------
# 2) Raster-cell fast mode
# ------------------------------------------------------
library(terra)
pu_mask <- rast("pu_mask.tif")
cost_r  <- rast("cost.tif")
feat_r  <- rast(c("sp1.tif", "sp2.tif"))

x <- inputData(
  pu = pu_mask,
  features = feat_r,
  cost = cost_r
)

# ------------------------------------------------------
# 3) Vector-PU spatial mode
# ------------------------------------------------------
pu_v   <- vect("pus.gpkg")
feat_r <- rast(c("sp1.tif", "sp2.tif"))
cost_r <- rast("cost.tif")

x <- inputData(
  pu = pu_v,
  features = feat_r,
  cost = cost_r,
  pu_id_col = "id",
  cost_aggregation = "mean"
)

# ------------------------------------------------------
# 4) Hybrid sf + tabular mode
# ------------------------------------------------------
library(sf)
pu_sf <- st_read("pus.gpkg")
features <- data.frame(id = 1:2, name = c("sp1", "sp2"))
dist_features <- data.frame(
  pu = c(1, 1, 2, 3),
  feature = c("sp1", "sp2", "sp2", "sp1"),
  amount = c(1, 5, 2, 1)
)

x <- inputData(
  pu = pu_sf,
  features = features,
  dist_features = dist_features,
  cost = "cost"
)
} # }
```
