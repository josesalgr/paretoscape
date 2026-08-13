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
# Load a complete simulated planning problem.
example_data <- load_sim_multiaction()

p <- create_problem(
  pu = example_data$planning_units,
  features = example_data$features,
  dist_features = example_data$dist_features,
  cost = "cost"
)

p <- add_spatial_distance(
  x = p,
  max_distance = 1.01,
  name = "within_1",
  weight_mode = "constant"
)

p$data$spatial_relations$within_1
#>     internal_pu1 internal_pu2 weight pu1 pu2 distance            source
#> 1              1            2      1   1   2        1 distance_constant
#> 2              2            3      1   2   3        1 distance_constant
#> 3              3            4      1   3   4        1 distance_constant
#> 4              4            5      1   4   5        1 distance_constant
#> 5              5            6      1   5   6        1 distance_constant
#> 6              6            7      1   6   7        1 distance_constant
#> 7              7            8      1   7   8        1 distance_constant
#> 8              1            9      1   1   9        1 distance_constant
#> 9              2           10      1   2  10        1 distance_constant
#> 10             9           10      1   9  10        1 distance_constant
#> 11             3           11      1   3  11        1 distance_constant
#> 12            10           11      1  10  11        1 distance_constant
#> 13             4           12      1   4  12        1 distance_constant
#> 14            11           12      1  11  12        1 distance_constant
#> 15             5           13      1   5  13        1 distance_constant
#> 16            12           13      1  12  13        1 distance_constant
#> 17             6           14      1   6  14        1 distance_constant
#> 18            13           14      1  13  14        1 distance_constant
#> 19             7           15      1   7  15        1 distance_constant
#> 20            14           15      1  14  15        1 distance_constant
#> 21             8           16      1   8  16        1 distance_constant
#> 22            15           16      1  15  16        1 distance_constant
#> 23             9           17      1   9  17        1 distance_constant
#> 24            10           18      1  10  18        1 distance_constant
#> 25            17           18      1  17  18        1 distance_constant
#> 26            11           19      1  11  19        1 distance_constant
#> 27            18           19      1  18  19        1 distance_constant
#> 28            12           20      1  12  20        1 distance_constant
#> 29            19           20      1  19  20        1 distance_constant
#> 30            13           21      1  13  21        1 distance_constant
#> 31            20           21      1  20  21        1 distance_constant
#> 32            14           22      1  14  22        1 distance_constant
#> 33            21           22      1  21  22        1 distance_constant
#> 34            15           23      1  15  23        1 distance_constant
#> 35            22           23      1  22  23        1 distance_constant
#> 36            16           24      1  16  24        1 distance_constant
#> 37            23           24      1  23  24        1 distance_constant
#> 38            17           25      1  17  25        1 distance_constant
#> 39            18           26      1  18  26        1 distance_constant
#> 40            25           26      1  25  26        1 distance_constant
#> 41            19           27      1  19  27        1 distance_constant
#> 42            26           27      1  26  27        1 distance_constant
#> 43            20           28      1  20  28        1 distance_constant
#> 44            27           28      1  27  28        1 distance_constant
#> 45            21           29      1  21  29        1 distance_constant
#> 46            28           29      1  28  29        1 distance_constant
#> 47            22           30      1  22  30        1 distance_constant
#> 48            29           30      1  29  30        1 distance_constant
#> 49            23           31      1  23  31        1 distance_constant
#> 50            30           31      1  30  31        1 distance_constant
#> 51            24           32      1  24  32        1 distance_constant
#> 52            31           32      1  31  32        1 distance_constant
#> 53            25           33      1  25  33        1 distance_constant
#> 54            26           34      1  26  34        1 distance_constant
#> 55            33           34      1  33  34        1 distance_constant
#> 56            27           35      1  27  35        1 distance_constant
#> 57            34           35      1  34  35        1 distance_constant
#> 58            28           36      1  28  36        1 distance_constant
#> 59            35           36      1  35  36        1 distance_constant
#> 60            29           37      1  29  37        1 distance_constant
#> 61            36           37      1  36  37        1 distance_constant
#> 62            30           38      1  30  38        1 distance_constant
#> 63            37           38      1  37  38        1 distance_constant
#> 64            31           39      1  31  39        1 distance_constant
#> 65            38           39      1  38  39        1 distance_constant
#> 66            32           40      1  32  40        1 distance_constant
#> 67            39           40      1  39  40        1 distance_constant
#> 68            33           41      1  33  41        1 distance_constant
#> 69            34           42      1  34  42        1 distance_constant
#> 70            41           42      1  41  42        1 distance_constant
#> 71            35           43      1  35  43        1 distance_constant
#> 72            42           43      1  42  43        1 distance_constant
#> 73            36           44      1  36  44        1 distance_constant
#> 74            43           44      1  43  44        1 distance_constant
#> 75            37           45      1  37  45        1 distance_constant
#> 76            44           45      1  44  45        1 distance_constant
#> 77            38           46      1  38  46        1 distance_constant
#> 78            45           46      1  45  46        1 distance_constant
#> 79            39           47      1  39  47        1 distance_constant
#> 80            46           47      1  46  47        1 distance_constant
#> 81            40           48      1  40  48        1 distance_constant
#> 82            47           48      1  47  48        1 distance_constant
#> 83            41           49      1  41  49        1 distance_constant
#> 84            42           50      1  42  50        1 distance_constant
#> 85            49           50      1  49  50        1 distance_constant
#> 86            43           51      1  43  51        1 distance_constant
#> 87            50           51      1  50  51        1 distance_constant
#> 88            44           52      1  44  52        1 distance_constant
#> 89            51           52      1  51  52        1 distance_constant
#> 90            45           53      1  45  53        1 distance_constant
#> 91            52           53      1  52  53        1 distance_constant
#> 92            46           54      1  46  54        1 distance_constant
#> 93            53           54      1  53  54        1 distance_constant
#> 94            47           55      1  47  55        1 distance_constant
#> 95            54           55      1  54  55        1 distance_constant
#> 96            48           56      1  48  56        1 distance_constant
#> 97            55           56      1  55  56        1 distance_constant
#> 98            49           57      1  49  57        1 distance_constant
#> 99            50           58      1  50  58        1 distance_constant
#> 100           57           58      1  57  58        1 distance_constant
#> 101           51           59      1  51  59        1 distance_constant
#> 102           58           59      1  58  59        1 distance_constant
#> 103           52           60      1  52  60        1 distance_constant
#> 104           59           60      1  59  60        1 distance_constant
#> 105           53           61      1  53  61        1 distance_constant
#> 106           60           61      1  60  61        1 distance_constant
#> 107           54           62      1  54  62        1 distance_constant
#> 108           61           62      1  61  62        1 distance_constant
#> 109           55           63      1  55  63        1 distance_constant
#> 110           62           63      1  62  63        1 distance_constant
#> 111           56           64      1  56  64        1 distance_constant
#> 112           63           64      1  63  64        1 distance_constant
#>     relation_name directed
#> 1        within_1    FALSE
#> 2        within_1    FALSE
#> 3        within_1    FALSE
#> 4        within_1    FALSE
#> 5        within_1    FALSE
#> 6        within_1    FALSE
#> 7        within_1    FALSE
#> 8        within_1    FALSE
#> 9        within_1    FALSE
#> 10       within_1    FALSE
#> 11       within_1    FALSE
#> 12       within_1    FALSE
#> 13       within_1    FALSE
#> 14       within_1    FALSE
#> 15       within_1    FALSE
#> 16       within_1    FALSE
#> 17       within_1    FALSE
#> 18       within_1    FALSE
#> 19       within_1    FALSE
#> 20       within_1    FALSE
#> 21       within_1    FALSE
#> 22       within_1    FALSE
#> 23       within_1    FALSE
#> 24       within_1    FALSE
#> 25       within_1    FALSE
#> 26       within_1    FALSE
#> 27       within_1    FALSE
#> 28       within_1    FALSE
#> 29       within_1    FALSE
#> 30       within_1    FALSE
#> 31       within_1    FALSE
#> 32       within_1    FALSE
#> 33       within_1    FALSE
#> 34       within_1    FALSE
#> 35       within_1    FALSE
#> 36       within_1    FALSE
#> 37       within_1    FALSE
#> 38       within_1    FALSE
#> 39       within_1    FALSE
#> 40       within_1    FALSE
#> 41       within_1    FALSE
#> 42       within_1    FALSE
#> 43       within_1    FALSE
#> 44       within_1    FALSE
#> 45       within_1    FALSE
#> 46       within_1    FALSE
#> 47       within_1    FALSE
#> 48       within_1    FALSE
#> 49       within_1    FALSE
#> 50       within_1    FALSE
#> 51       within_1    FALSE
#> 52       within_1    FALSE
#> 53       within_1    FALSE
#> 54       within_1    FALSE
#> 55       within_1    FALSE
#> 56       within_1    FALSE
#> 57       within_1    FALSE
#> 58       within_1    FALSE
#> 59       within_1    FALSE
#> 60       within_1    FALSE
#> 61       within_1    FALSE
#> 62       within_1    FALSE
#> 63       within_1    FALSE
#> 64       within_1    FALSE
#> 65       within_1    FALSE
#> 66       within_1    FALSE
#> 67       within_1    FALSE
#> 68       within_1    FALSE
#> 69       within_1    FALSE
#> 70       within_1    FALSE
#> 71       within_1    FALSE
#> 72       within_1    FALSE
#> 73       within_1    FALSE
#> 74       within_1    FALSE
#> 75       within_1    FALSE
#> 76       within_1    FALSE
#> 77       within_1    FALSE
#> 78       within_1    FALSE
#> 79       within_1    FALSE
#> 80       within_1    FALSE
#> 81       within_1    FALSE
#> 82       within_1    FALSE
#> 83       within_1    FALSE
#> 84       within_1    FALSE
#> 85       within_1    FALSE
#> 86       within_1    FALSE
#> 87       within_1    FALSE
#> 88       within_1    FALSE
#> 89       within_1    FALSE
#> 90       within_1    FALSE
#> 91       within_1    FALSE
#> 92       within_1    FALSE
#> 93       within_1    FALSE
#> 94       within_1    FALSE
#> 95       within_1    FALSE
#> 96       within_1    FALSE
#> 97       within_1    FALSE
#> 98       within_1    FALSE
#> 99       within_1    FALSE
#> 100      within_1    FALSE
#> 101      within_1    FALSE
#> 102      within_1    FALSE
#> 103      within_1    FALSE
#> 104      within_1    FALSE
#> 105      within_1    FALSE
#> 106      within_1    FALSE
#> 107      within_1    FALSE
#> 108      within_1    FALSE
#> 109      within_1    FALSE
#> 110      within_1    FALSE
#> 111      within_1    FALSE
#> 112      within_1    FALSE
```
