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
  dmax,
  name = "distance",
  weight_fn = c("constant", "inverse", "inverse_sq"),
  eps = 1e-09
)
```

## Arguments

- x:

  A `Problem` object created with
  [`inputData`](https://josesalgr.github.io/mosap/reference/inputData.md).

- coords:

  Optional coordinates specification, following the same rules as in
  [`add_spatial_knn`](https://josesalgr.github.io/mosap/reference/add_spatial_knn.md).

- dmax:

  Positive numeric scalar giving the maximum distance for an edge.

- name:

  Character string giving the key under which the relation is stored.

- weight_fn:

  Character string indicating how distance is converted to weight. Must
  be one of `"constant"`, `"inverse"`, or `"inverse_sq"`.

- eps:

  Small positive numeric constant used to avoid division by zero in
  inverse-distance weighting.

## Value

An updated `Problem` object.

## Details

Let \\s_i = (x_i, y_i)\\ denote the coordinates of planning unit \\i\\.
Let \\d\_{ij}\\ be the Euclidean distance between planning units \\i\\
and \\j\\.

For a user-supplied threshold \\d\_{\max}\\, this constructor creates an
edge between \\i\\ and \\j\\ whenever: \$\$ d\_{ij} \le d\_{\max}. \$\$

Edge weights are assigned according to `weight_fn`:

- `"constant"`: \$\$\omega\_{ij} = 1,\$\$

- `"inverse"`: \$\$\omega\_{ij} = \frac{1}{\max(d\_{ij},
  \varepsilon)},\$\$

- `"inverse_sq"`: \$\$\omega\_{ij} = \frac{1}{\max(d\_{ij},
  \varepsilon)^2},\$\$

where \\\varepsilon = \code{eps}\\ is a small constant.

The implementation computes an \\O(n^2)\\ distance matrix and is
therefore best suited to small or moderate numbers of planning units.
For large problems,
[`add_spatial_knn`](https://josesalgr.github.io/mosap/reference/add_spatial_knn.md)
is often more scalable.

The resulting relation is registered as undirected.

## See also

[`add_spatial_knn`](https://josesalgr.github.io/mosap/reference/add_spatial_knn.md),
[`add_spatial_relations`](https://josesalgr.github.io/mosap/reference/add_spatial_relations.md)

## Examples

``` r
if (FALSE) { # \dontrun{
p <- add_spatial_distance(
  x = p,
  dmax = 1000,
  name = "within_1km",
  weight_fn = "constant"
)
} # }
```
