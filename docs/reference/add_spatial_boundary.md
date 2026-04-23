# Add spatial boundary-length relations

Build and register a boundary-length spatial relation between planning
units.

Boundary relations represent shared edge length between adjacent
polygons. In contrast to queen adjacency, they only account for boundary
segments of positive length and ignore point-only contacts.

## Usage

``` r
add_spatial_boundary(
  x,
  boundary = NULL,
  geometry = NULL,
  name = "boundary",
  weight_col = NULL,
  weight_multiplier = 1,
  include_self = TRUE,
  edge_factor = 1
)
```

## Arguments

- x:

  A `Problem` object.

- boundary:

  Optional `data.frame` describing boundary lengths. Accepted formats
  are:

  - `(id1, id2, boundary)`, or

  - `(pu1, pu2, weight)`.

- geometry:

  Optional `sf` object with planning-unit polygons and an `id` column.
  If `NULL`, `x$data$pu_sf` is used.

- name:

  Character string giving the key under which the relation is stored.

- weight_col:

  Optional character string giving the name of the weight column in
  `boundary`. If `NULL`, the function tries to infer it from
  `"boundary"` or `"weight"`.

- weight_multiplier:

  Positive numeric scalar applied to all boundary weights.

- include_self:

  Logical. If `TRUE`, include diagonal entries representing exposed
  boundary.

- edge_factor:

  Numeric scalar greater than or equal to zero. Multiplier applied to
  exposed boundary when constructing diagonal entries.

## Value

An updated `Problem` object with the stored relation in
`x$data$spatial_relations[[name]]`.

## Details

Use this function when spatial structure should be represented through
shared boundary length rather than binary contiguity or coordinate-based
proximity.

Two input modes are supported:

1.  **Boundary-table mode.** If `boundary` is supplied, it is
    interpreted as a boundary table, for example a Marxan-style
    `bound.dat`.

2.  **Geometry mode.** If `boundary = NULL`, boundary lengths are
    derived from polygon geometry using `geometry` or `x$data$pu_sf`.

Let \\\omega\_{ij} \ge 0\\ denote the shared boundary length between
planning units \\i\\ and \\j\\, multiplied by `weight_multiplier`.

For off-diagonal entries \\i \neq j\\, the stored weight is: \$\$
\omega\_{ij} = \mathrm{\gamma} \times b\_{ij}, \$\$ where \\b\_{ij}\\ is
the shared boundary length and \\\gamma\\ is the user-supplied
`weight_multiplier`.

If `include_self = TRUE`, diagonal entries are also created. These are
not geometric self-neighbours in the graph sense; instead, they
represent the effective boundary exposed to the outside of the solution.

Let \\p_i\\ be the total perimeter of planning unit \\i\\, and let
\\\sum\_{j \neq i} \omega\_{ij}\\ be the total incident shared boundary
recorded for that planning unit. Then the exposed boundary is
represented by a diagonal term derived from the difference between total
perimeter and shared boundary, scaled by `edge_factor`.

These diagonal terms are useful in boundary-based compactness or
fragmentation objectives, because they encode the portion of each
planning unit's perimeter that would remain exposed if the unit were
selected.

**Boundary-table mode**

If `boundary` is provided, accepted formats are:

- `(id1, id2, boundary)`, or

- `(pu1, pu2, weight)`.

If the table contains diagonal rows \\(i,i)\\, these are interpreted as
total perimeter values in boundary-table mode.

**Geometry mode**

If `boundary = NULL`, shared boundary lengths are derived directly from
polygon geometry. Only positive-length intersections are retained. Point
touches are ignored.

**Storage**

The final relation is stored through
[`add_spatial_relations`](https://josesalgr.github.io/multiscape/reference/add_spatial_relations.md),
typically as an undirected relation with optional diagonal entries.

## See also

[`add_spatial_relations`](https://josesalgr.github.io/multiscape/reference/add_spatial_relations.md),
[`add_objective_min_fragmentation_pu`](https://josesalgr.github.io/multiscape/reference/add_objective_min_fragmentation_pu.md),
[`add_objective_min_fragmentation_action`](https://josesalgr.github.io/multiscape/reference/add_objective_min_fragmentation_action.md)

## Examples

``` r
pu_tbl <- data.frame(
  id = 1:4,
  cost = c(1, 2, 3, 4)
)

feat_tbl <- data.frame(
  id = 1:2,
  name = c("feature_1", "feature_2")
)

dist_feat_tbl <- data.frame(
  pu = c(1, 1, 2, 3, 4),
  feature = c(1, 2, 2, 1, 2),
  amount = c(5, 2, 3, 4, 1)
)

bound_df <- data.frame(
  id1 = c(1, 1, 2, 1, 2, 3, 4),
  id2 = c(1, 2, 2, 3, 4, 4, 4),
  boundary = c(4, 1, 4, 1, 1, 1, 4)
)

p <- create_problem(
  pu = pu_tbl,
  features = feat_tbl,
  dist_features = dist_feat_tbl,
  cost = "cost"
)

p <- add_spatial_boundary(
  x = p,
  boundary = bound_df,
  name = "boundary",
  include_self = TRUE,
  edge_factor = 1
)

p$data$spatial_relations$boundary
#>   internal_pu1 internal_pu2 weight pu1 pu2                        source
#> 1            1            2      1   1   2         boundary_table_shared
#> 2            1            3      1   1   3         boundary_table_shared
#> 3            2            4      1   2   4         boundary_table_shared
#> 4            3            4      1   3   4         boundary_table_shared
#> 5            1            1      2   1   1 boundary_table_diag_effective
#> 6            2            2      2   2   2 boundary_table_diag_effective
#> 7            3            3     -2   3   3 boundary_table_diag_effective
#> 8            4            4      2   4   4 boundary_table_diag_effective
#>   relation_name
#> 1      boundary
#> 2      boundary
#> 3      boundary
#> 4      boundary
#> 5      boundary
#> 6      boundary
#> 7      boundary
#> 8      boundary
```
