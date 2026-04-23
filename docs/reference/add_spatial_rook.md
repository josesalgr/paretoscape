# Add rook adjacency from polygons

Build and register a rook adjacency relation from planning-unit
polygons.

Two planning units are rook-adjacent if they share a boundary segment of
positive length. Corner-only contact does not count as rook adjacency.

## Usage

``` r
add_spatial_rook(x, geometry = NULL, name = "rook", weight = 1)
```

## Arguments

- x:

  A `Problem` object created with
  [`create_problem`](https://josesalgr.github.io/multiscape/reference/create_problem.md)
  or another object containing aligned planning-unit polygons.

- geometry:

  Optional `sf` object with planning-unit polygons and an `id` column.
  If `NULL`, `x$data$pu_sf` is used.

- name:

  Character string giving the key under which the relation is stored.

- weight:

  Numeric scalar giving the edge weight assigned to each rook adjacency.

## Value

An updated `Problem` object.

## Details

Use this function when neighbourhood should be defined by shared polygon
edges rather than by point-touching or coordinate-based proximity.

This constructor derives an adjacency graph from polygon geometry using
a rook criterion. If planning units \\i\\ and \\j\\ share a common edge
of non-zero length, then an edge \\(i,j)\\ is added to the relation.

Let \\G = (\mathcal{I}, E)\\ denote the resulting graph. Then: \$\$
(i,j) \in E \quad \Longleftrightarrow \quad \mathrm{length}(\partial i
\cap \partial j) \> 0. \$\$

All edges receive the same user-supplied weight.

The resulting relation is stored as an undirected spatial relation.

## See also

[`add_spatial_queen`](https://josesalgr.github.io/multiscape/reference/add_spatial_queen.md),
[`add_spatial_boundary`](https://josesalgr.github.io/multiscape/reference/add_spatial_boundary.md)

## Examples

``` r
# \donttest{
library(terra)

data("sim_pu_sf", package = "multiscape")
sim_features <- load_sim_features_raster()

p <- create_problem(
  pu = sim_pu_sf,
  features = sim_features,
  cost = "cost"
)

p <- add_spatial_rook(
  x = p,
  geometry = sim_pu_sf,
  name = "rook",
  weight = 1
)

head(p$data$spatial_relations$rook)
#>       internal_pu1 internal_pu2 weight   pu1   pu2  source relation_name
#> 54596        10000        10001      1 10000 10001 rook_sf          rook
#> 54597        10000        10132      1 10000 10132 rook_sf          rook
#> 54598        10000        10133      1 10000 10133 rook_sf          rook
#> 54602        10001        10002      1 10001 10002 rook_sf          rook
#> 54603        10001        10133      1 10001 10133 rook_sf          rook
#> 54604        10001        10134      1 10001 10134 rook_sf          rook
# }
```
