# Add k-nearest-neighbours spatial relations

Build and register a k-nearest-neighbours graph between planning units
using coordinates.

This constructor does not require polygon geometry. It uses
planning-unit coordinates supplied explicitly or stored in the `Problem`
object.

## Usage

``` r
add_spatial_knn(
  x,
  coords = NULL,
  k = 8,
  name = "knn",
  weight_mode = c("constant", "inverse", "inverse_sq"),
  distance_eps = 1e-09
)
```

## Arguments

- x:

  A `Problem` object created with
  [`create_problem`](https://josesalgr.github.io/multiscape/reference/create_problem.md).

- coords:

  Optional coordinates specification. This may be:

  - a `data.frame(id, x, y)`, or

  - a numeric matrix with two columns `(x, y)` aligned to the order of
    planning units.

  If `NULL`, coordinates are taken from `x$data$pu_coords` or from
  columns `x$data$pu$x` and `x$data$pu$y`.

- k:

  Integer giving the number of neighbours per planning unit. Must be at
  least 1 and strictly less than the number of planning units.

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

Use this function when neighbourhood should be defined by a fixed number
of nearby planning units rather than by polygon topology or a fixed
distance threshold.

Let \\s_i = (x_i, y_i)\\ denote the coordinates of planning unit \\i\\.
For each planning unit, this function identifies the `k` nearest
distinct planning units under Euclidean distance.

If \\d\_{ij}\\ denotes the Euclidean distance between units \\i\\ and
\\j\\, then the k-nearest-neighbours relation is constructed by adding
an edge from \\i\\ to each of its `k` nearest neighbours.

Edge weights are then assigned according to `weight_mode`:

- `"constant"`: \$\$\omega\_{ij} = 1,\$\$

- `"inverse"`: \$\$\omega\_{ij} = \frac{1}{\max(d\_{ij},
  \varepsilon)},\$\$

- `"inverse_sq"`: \$\$\omega\_{ij} = \frac{1}{\max(d\_{ij},
  \varepsilon)^2},\$\$

where \\\varepsilon\\ = `distance_eps` is a small constant to avoid
division by zero.

The raw k-nearest-neighbours structure is directional by construction,
but the stored relation is registered as undirected by default through
[`add_spatial_relations`](https://josesalgr.github.io/multiscape/reference/add_spatial_relations.md),
which collapses duplicate unordered pairs.

If the RANN package is available, it is used for efficient nearest
neighbour search. Otherwise, a full distance matrix is computed.

## See also

[`add_spatial_distance`](https://josesalgr.github.io/multiscape/reference/add_spatial_distance.md),
[`add_spatial_relations`](https://josesalgr.github.io/multiscape/reference/add_spatial_relations.md)

## Examples

``` r
# Load a complete simulated planning problem.
example_data <- load_sim_multiaction()

p <- create_problem(
  pu = example_data$planning_units,
  features = example_data$features,
  dist_features = example_data$dist_features,
  cost = "cost"
)

p <- add_spatial_knn(
  x = p,
  k = 2,
  name = "knn2",
  weight_mode = "constant"
)

p$data$spatial_relations$knn2
#>     internal_pu1 internal_pu2 weight pu1 pu2 distance       source
#> 1              1            9      1   1   9        1 knn_constant
#> 2              1            2      1   1   2        1 knn_constant
#> 3              2           10      1   2  10        1 knn_constant
#> 5              3           11      1   3  11        1 knn_constant
#> 6              3            4      1   3   4        1 knn_constant
#> 7              4           12      1   4  12        1 knn_constant
#> 9              5           13      1   5  13        1 knn_constant
#> 10             5            6      1   5   6        1 knn_constant
#> 11             6           14      1   6  14        1 knn_constant
#> 13             7           15      1   7  15        1 knn_constant
#> 14             7            8      1   7   8        1 knn_constant
#> 15             8           16      1   8  16        1 knn_constant
#> 18             9           10      1   9  10        1 knn_constant
#> 22            11           12      1  11  12        1 knn_constant
#> 26            13           14      1  13  14        1 knn_constant
#> 30            15           16      1  15  16        1 knn_constant
#> 33            17           25      1  17  25        1 knn_constant
#> 34            17           18      1  17  18        1 knn_constant
#> 35            18           26      1  18  26        1 knn_constant
#> 37            19           27      1  19  27        1 knn_constant
#> 38            19           20      1  19  20        1 knn_constant
#> 39            20           28      1  20  28        1 knn_constant
#> 41            21           29      1  21  29        1 knn_constant
#> 42            21           22      1  21  22        1 knn_constant
#> 43            22           30      1  22  30        1 knn_constant
#> 45            23           31      1  23  31        1 knn_constant
#> 46            23           24      1  23  24        1 knn_constant
#> 47            24           32      1  24  32        1 knn_constant
#> 50            25           26      1  25  26        1 knn_constant
#> 54            27           28      1  27  28        1 knn_constant
#> 58            29           30      1  29  30        1 knn_constant
#> 62            31           32      1  31  32        1 knn_constant
#> 65            33           41      1  33  41        1 knn_constant
#> 66            33           34      1  33  34        1 knn_constant
#> 67            34           42      1  34  42        1 knn_constant
#> 69            35           43      1  35  43        1 knn_constant
#> 70            35           36      1  35  36        1 knn_constant
#> 71            36           44      1  36  44        1 knn_constant
#> 73            37           45      1  37  45        1 knn_constant
#> 74            37           38      1  37  38        1 knn_constant
#> 75            38           46      1  38  46        1 knn_constant
#> 77            39           47      1  39  47        1 knn_constant
#> 78            39           40      1  39  40        1 knn_constant
#> 79            40           48      1  40  48        1 knn_constant
#> 82            41           42      1  41  42        1 knn_constant
#> 86            43           44      1  43  44        1 knn_constant
#> 90            45           46      1  45  46        1 knn_constant
#> 94            47           48      1  47  48        1 knn_constant
#> 97            49           57      1  49  57        1 knn_constant
#> 98            49           50      1  49  50        1 knn_constant
#> 99            50           58      1  50  58        1 knn_constant
#> 101           51           59      1  51  59        1 knn_constant
#> 102           51           52      1  51  52        1 knn_constant
#> 103           52           60      1  52  60        1 knn_constant
#> 105           53           61      1  53  61        1 knn_constant
#> 106           53           54      1  53  54        1 knn_constant
#> 107           54           62      1  54  62        1 knn_constant
#> 109           55           63      1  55  63        1 knn_constant
#> 110           55           56      1  55  56        1 knn_constant
#> 111           56           64      1  56  64        1 knn_constant
#> 114           57           58      1  57  58        1 knn_constant
#> 118           59           60      1  59  60        1 knn_constant
#> 122           61           62      1  61  62        1 knn_constant
#> 126           63           64      1  63  64        1 knn_constant
#>     relation_name directed
#> 1            knn2    FALSE
#> 2            knn2    FALSE
#> 3            knn2    FALSE
#> 5            knn2    FALSE
#> 6            knn2    FALSE
#> 7            knn2    FALSE
#> 9            knn2    FALSE
#> 10           knn2    FALSE
#> 11           knn2    FALSE
#> 13           knn2    FALSE
#> 14           knn2    FALSE
#> 15           knn2    FALSE
#> 18           knn2    FALSE
#> 22           knn2    FALSE
#> 26           knn2    FALSE
#> 30           knn2    FALSE
#> 33           knn2    FALSE
#> 34           knn2    FALSE
#> 35           knn2    FALSE
#> 37           knn2    FALSE
#> 38           knn2    FALSE
#> 39           knn2    FALSE
#> 41           knn2    FALSE
#> 42           knn2    FALSE
#> 43           knn2    FALSE
#> 45           knn2    FALSE
#> 46           knn2    FALSE
#> 47           knn2    FALSE
#> 50           knn2    FALSE
#> 54           knn2    FALSE
#> 58           knn2    FALSE
#> 62           knn2    FALSE
#> 65           knn2    FALSE
#> 66           knn2    FALSE
#> 67           knn2    FALSE
#> 69           knn2    FALSE
#> 70           knn2    FALSE
#> 71           knn2    FALSE
#> 73           knn2    FALSE
#> 74           knn2    FALSE
#> 75           knn2    FALSE
#> 77           knn2    FALSE
#> 78           knn2    FALSE
#> 79           knn2    FALSE
#> 82           knn2    FALSE
#> 86           knn2    FALSE
#> 90           knn2    FALSE
#> 94           knn2    FALSE
#> 97           knn2    FALSE
#> 98           knn2    FALSE
#> 99           knn2    FALSE
#> 101          knn2    FALSE
#> 102          knn2    FALSE
#> 103          knn2    FALSE
#> 105          knn2    FALSE
#> 106          knn2    FALSE
#> 107          knn2    FALSE
#> 109          knn2    FALSE
#> 110          knn2    FALSE
#> 111          knn2    FALSE
#> 114          knn2    FALSE
#> 118          knn2    FALSE
#> 122          knn2    FALSE
#> 126          knn2    FALSE
```
