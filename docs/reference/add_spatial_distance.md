# Add distance-threshold spatial relations

Build and register a spatial relation connecting planning units whose
Euclidean distance is less than or equal to a user-defined threshold.

This constructor does not require polygon geometry and instead uses
planning-unit coordinates.

## Usage

``` r
add_spatial_distance(
  x,
  coords = NULL,
  max_distance,
  name = "distance",
  weight_mode = c("constant", "inverse", "inverse_sq"),
  distance_eps = 1e-09
)
```

## Arguments

- x:

  A `Problem` object created with
  [`create_problem`](https://josesalgr.github.io/multiscape/reference/create_problem.md).

- coords:

  Optional coordinates specification, following the same rules as in
  [`add_spatial_knn`](https://josesalgr.github.io/multiscape/reference/add_spatial_knn.md).

- max_distance:

  Positive numeric scalar giving the maximum distance for an edge.

- name:

  Character string giving the key under which the relation is stored.

- weight_mode:

  Character string indicating how distance is converted to weight. Must
  be one of `"constant"`, `"inverse"`, or `"inverse_sq"`.

- distance_eps:

  Small positive numeric constant used to avoid division by zero in
  inverse-distance weighting.

## Value

An updated `Problem` object.

## Details

Use this function when neighbourhood should be defined by a fixed
distance radius rather than by polygon topology or a fixed number of
neighbours.

Let \\s_i = (x_i, y_i)\\ denote the coordinates of planning unit \\i\\.
Let \\d\_{ij}\\ be the Euclidean distance between planning units \\i\\
and \\j\\.

For a user-supplied threshold \\d\_{\max}\\, this constructor creates an
edge between \\i\\ and \\j\\ whenever: \$\$ d\_{ij} \le d\_{\max}. \$\$

Edge weights are assigned according to `weight_mode`:

- `"constant"`: \$\$\omega\_{ij} = 1,\$\$

- `"inverse"`: \$\$\omega\_{ij} = \frac{1}{\max(d\_{ij},
  \varepsilon)},\$\$

- `"inverse_sq"`: \$\$\omega\_{ij} = \frac{1}{\max(d\_{ij},
  \varepsilon)^2},\$\$

where \\\varepsilon\\ = `distance_eps` is a small constant.

The implementation computes an \\O(n^2)\\ distance matrix and is
therefore best suited to small or moderate numbers of planning units.
For large problems,
[`add_spatial_knn`](https://josesalgr.github.io/multiscape/reference/add_spatial_knn.md)
is often more scalable.

The resulting relation is registered as undirected.

## See also

[`add_spatial_knn`](https://josesalgr.github.io/multiscape/reference/add_spatial_knn.md),
[`add_spatial_relations`](https://josesalgr.github.io/multiscape/reference/add_spatial_relations.md)

## Examples

``` r
pu_tbl <- data.frame(
  id = 1:4,
  cost = c(1, 2, 3, 4),
  x = c(0, 1, 0, 1),
  y = c(0, 0, 1, 1)
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

p <- create_problem(
  pu = pu_tbl,
  features = feat_tbl,
  dist_features = dist_feat_tbl,
  cost = "cost"
)

p <- add_spatial_distance(
  x = p,
  max_distance = 1.01,
  name = "within_1",
  weight_mode = "constant"
)

p$data$spatial_relations$within_1
#>   internal_pu1 internal_pu2 weight pu1 pu2 distance            source
#> 1            1            2      1   2   1        1 distance_constant
#> 2            1            3      1   3   1        1 distance_constant
#> 4            2            4      1   4   2        1 distance_constant
#> 6            3            4      1   4   3        1 distance_constant
#>   relation_name
#> 1      within_1
#> 2      within_1
#> 4      within_1
#> 6      within_1
```
