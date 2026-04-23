# Add queen adjacency from polygons

Build and register a queen adjacency relation from planning-unit
polygons.

Two planning units are queen-adjacent if their boundaries touch, either
along a shared edge or at a shared vertex.

## Usage

``` r
add_spatial_queen(x, geometry = NULL, name = "queen", weight = 1)
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

  Numeric scalar giving the edge weight assigned to each queen
  adjacency.

## Value

An updated `Problem` object.

## Details

Use this function when neighbourhood should include both shared edges
and corner-touching polygon contacts.

This constructor derives an adjacency graph from polygon geometry using
a queen criterion. If planning units \\i\\ and \\j\\ touch at any
boundary point, then an edge \\(i,j)\\ is added to the relation.

Let \\G = (\mathcal{I}, E)\\ denote the resulting graph. Then: \$\$
(i,j) \in E \quad \Longleftrightarrow \quad \partial i \cap \partial j
\neq \varnothing. \$\$

Thus, queen adjacency includes all rook neighbours plus corner-touching
neighbours.

All edges receive the same user-supplied weight.

The resulting relation is stored as an undirected spatial relation.

## See also

[`add_spatial_rook`](https://josesalgr.github.io/multiscape/reference/add_spatial_rook.md),
[`add_spatial_boundary`](https://josesalgr.github.io/multiscape/reference/add_spatial_boundary.md)

## Examples

``` r
# \donttest{
library(terra)
#> Warning: package 'terra' was built under R version 4.4.3
#> terra 1.8.80

data("sim_pu_sf", package = "multiscape")
sim_features <- load_sim_features_raster()

p <- create_problem(
  pu = sim_pu_sf,
  features = sim_features,
  cost = "cost"
)

p <- add_spatial_queen(
  x = p,
  geometry = sim_pu_sf,
  name = "queen",
  weight = 1
)

head(p$data$spatial_relations$queen)
#>       internal_pu1 internal_pu2 weight   pu1   pu2   source relation_name
#> 27430        10000        10001      1 10000 10001 queen_sf         queen
#> 27431        10000        10132      1 10000 10132 queen_sf         queen
#> 27432        10000        10133      1 10000 10133 queen_sf         queen
#> 27433        10001        10002      1 10001 10002 queen_sf         queen
#> 27434        10001        10133      1 10001 10133 queen_sf         queen
#> 27435        10001        10134      1 10001 10134 queen_sf         queen
# }
```
