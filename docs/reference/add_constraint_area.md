# Add area constraint

Add a total selected area constraint to a planning problem.

This function stores an area constraint in the `Problem` object so that
it can be incorporated later by the model builder when the optimization
model is assembled.

## Usage

``` r
add_constraint_area(
  x,
  area,
  sense,
  tolerance = 0,
  area_col = NULL,
  area_unit = c("m2", "ha", "km2"),
  name = "area"
)
```

## Arguments

- x:

  A `Problem` object.

- area:

  Numeric scalar greater than or equal to zero. Target value for the
  total selected area.

- sense:

  Character string indicating the type of area constraint. Must be one
  of `"min"`, `"max"`, or `"equal"`.

- tolerance:

  Numeric scalar greater than or equal to zero. Only used when
  `sense = "equal"`. In that case, the equality is interpreted as a band
  around `area` with half-width `tolerance`. Ignored otherwise.

- area_col:

  Optional character string giving the name of the area column in
  `x$data$pu`. If `NULL`, the area source is resolved later by the model
  builder.

- area_unit:

  Character string indicating the unit of `area`. Must be one of `"m2"`,
  `"ha"`, or `"km2"`.

- name:

  Character string used as the label of the stored linear constraint
  when it is later added to the optimization model.

## Value

An updated `Problem` object with a stored area constraint in
`x$data$constraints$area`.

## Details

Let \\\mathcal{P}\\ denote the set of planning units and let \\a_i \ge
0\\ be the area associated with planning unit \\i \in \mathcal{P}\\. Let
\\w_i \in \\0,1\\\\ denote the binary variable indicating whether
planning unit \\i\\ is selected by at least one decision in the model.

Depending on `sense`, this function stores one of the following
constraints:

If `sense = "min"`: \$\$ \sum\_{i \in \mathcal{P}} a_i w_i \ge A \$\$

If `sense = "max"`: \$\$ \sum\_{i \in \mathcal{P}} a_i w_i \le A \$\$

If `sense = "equal"` and `tolerance = 0`: \$\$ \sum\_{i \in \mathcal{P}}
a_i w_i = A \$\$

If `sense = "equal"` and `tolerance > 0`: \$\$ A - \tau \le \sum\_{i \in
\mathcal{P}} a_i w_i \le A + \tau \$\$ where \\\tau\\ is the value
supplied through `tolerance`.

Areas are obtained from `x$data$pu`. If `area_col` is provided, that
column is used. Otherwise, the model builder will later determine the
default area source according to the internal rules of the package. The
value of `area_unit` indicates the unit in which `area` and `tolerance`
are expressed and therefore how the stored threshold should be
interpreted.

This function only stores the constraint specification in
`x$data$constraints$area`; it does not validate the feasibility of the
threshold against the available planning units at this stage.

At most one area constraint can be stored in a `Problem` object. If one
already exists, this function raises an error.

## See also

[`create_problem`](https://josesalgr.github.io/multiscape/reference/create_problem.md)

## Examples

``` r
pu <- data.frame(
  id = 1:4,
  cost = c(2, 3, 1, 4),
  area_ha = c(10, 15, 8, 20)
)

features <- data.frame(
  id = 1:2,
  name = c("sp1", "sp2")
)

dist_features <- data.frame(
  pu = c(1, 1, 2, 3, 4, 4),
  feature = c(1, 2, 1, 2, 1, 2),
  amount = c(1, 2, 1, 3, 2, 1)
)

p <- create_problem(
  pu = pu,
  features = features,
  dist_features = dist_features
)

p <- add_constraint_area(
  x = p,
  area = 25,
  sense = "min",
  area_col = "area_ha",
  area_unit = "ha"
)

p$data$constraints$area
#> $type
#> [1] "area"
#> 
#> $sense
#> [1] "min"
#> 
#> $value
#> [1] 25
#> 
#> $tolerance
#> [1] 0
#> 
#> $unit
#> [1] "ha"
#> 
#> $area_col
#> [1] "area_ha"
#> 
#> $name
#> [1] "area"
#> 
```
