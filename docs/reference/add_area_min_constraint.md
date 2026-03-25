# Add minimum selected area constraint

Add a lower bound on the total selected area in a planning problem.

This function stores a minimum-area constraint in the `Problem` object
so that it can be incorporated later by the model builder when the
optimization model is assembled.

## Usage

``` r
add_area_min_constraint(
  x,
  area_min,
  area_col = NULL,
  area_unit = c("m2", "ha", "km2"),
  name = "area_min",
  overwrite = FALSE
)
```

## Arguments

- x:

  A `Problem` object.

- area_min:

  Numeric scalar greater than or equal to zero. Minimum total selected
  area required in the solution.

- area_col:

  Optional character string giving the name of the area column in
  `x$data$pu`. If `NULL`, the area source is resolved later by the model
  builder.

- area_unit:

  Character string indicating the unit of `area_min`. Must be one of
  `"m2"`, `"ha"`, or `"km2"`.

- name:

  Character string used as a label for the stored constraint.

- overwrite:

  Logical. If `TRUE`, replace an existing stored minimum area
  constraint.

## Value

An updated `Problem` object with a stored minimum-area constraint in
`x$data$constraints$area_min`.

## Details

Let \\\mathcal{P}\\ denote the set of planning units and let \\a_i \ge
0\\ be the area associated with planning unit \\i \in \mathcal{P}\\. Let
\\w_i \in \\0,1\\\\ denote the binary variable indicating whether
planning unit \\i\\ is selected by at least one decision in the model.

This function stores the following constraint:

\$\$ \sum\_{i \in \mathcal{P}} a_i w_i \ge A\_{\min}, \$\$

where \\A\_{\min}\\ is the value supplied through `area_min`.

Areas are obtained from `x$data$pu`. If `area_col` is provided, that
column is used. Otherwise, the model builder will later determine the
default area source according to the internal rules of the package. The
value of `area_unit` indicates the unit in which `area_min` is expressed
and therefore how the stored threshold should be interpreted.

This function only stores the constraint specification in
`x$data$constraints$area_min`; it does not validate the feasibility of
the threshold against the available planning units at this stage.

If a minimum-area constraint has already been stored, it is replaced
only when `overwrite = TRUE`.

## See also

[`add_area_max_constraint`](https://josesalgr.github.io/mosap/reference/add_area_max_constraint.md),
[`inputData`](https://josesalgr.github.io/mosap/reference/inputData.md),

## Examples

``` r
pu <- data.frame(
  id = 1:4,
  cost = c(2, 3, 1, 4),
  area_ha = c(10, 15, 8, 20)
)

features <- data.frame(
  id = c("sp1", "sp2")
)

dist_features <- data.frame(
  pu = c(1, 1, 2, 3, 4, 4),
  feature = c("sp1", "sp2", "sp1", "sp2", "sp1", "sp2"),
  amount = c(1, 2, 1, 3, 2, 1)
)

p <- inputData(
  pu = pu,
  features = features,
  dist_features = dist_features
)
#> Error: features$id must be numeric/integer ids (got non-numeric strings).

p <- add_area_min_constraint(
  x = p,
  area_min = 25,
  area_col = "area_ha",
  area_unit = "ha"
)
#> Error: object 'p' not found

p$data$constraints$area_min
#> Error: object 'p' not found
```
