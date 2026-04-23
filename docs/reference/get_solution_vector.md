# Get raw solution vector from a solution

Return the raw decision-variable vector produced by the solver, in the
internal model-variable order used by the optimization backend.

## Usage

``` r
get_solution_vector(x, run = 1L)
```

## Arguments

- x:

  A `Solution` or `SolutionSet` object returned by
  [`solve`](https://josesalgr.github.io/multiscape/reference/solve.md).

- run:

  Positive integer giving the run index to extract when `x` is a
  `SolutionSet`. Default is `1L`.

## Value

A numeric vector with one value per internal model variable.

## Details

This function extracts the raw solution vector stored at
`x$solution$vector` for a `Solution` or for a selected run of a
`SolutionSet`.

The returned vector is in the internal variable order of the
optimization model. Depending on the problem formulation, it may
include:

- planning-unit selection variables,

- action-allocation variables,

- auxiliary variables introduced for targets, budgets, fragmentation, or
  other constraints/objectives,

- and potentially additional blocks created internally by the model
  builder.

Therefore, this vector is primarily intended for advanced users,
debugging, diagnostics, or internal verification. It is not a
user-facing allocation table.

To inspect selected planning units or selected actions in a more
interpretable form, use
[`get_pu`](https://josesalgr.github.io/multiscape/reference/get_pu.md)
or
[`get_actions`](https://josesalgr.github.io/multiscape/reference/get_actions.md)
instead.

For a single solution, the returned vector corresponds to that solution.
For a `SolutionSet`, the `run` argument selects which run to extract.

## See also

[`get_pu`](https://josesalgr.github.io/multiscape/reference/get_pu.md),
[`get_actions`](https://josesalgr.github.io/multiscape/reference/get_actions.md),
[`get_features`](https://josesalgr.github.io/multiscape/reference/get_features.md),
[`get_targets`](https://josesalgr.github.io/multiscape/reference/get_targets.md)

## Examples

``` r
# \donttest{
if (requireNamespace("rcbc", quietly = TRUE)) {
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

  actions_df <- data.frame(
    id = "conservation",
    name = "conservation"
  )

  effects_df <- data.frame(
    pu = c(1, 2, 3, 4),
    action = "conservation",
    feature = c(1, 1, 2, 2),
    benefit = c(2, 1, 1, 2),
    loss = c(0, 0, 0, 0)
  )

  p <- create_problem(
    pu = pu_tbl,
    features = feat_tbl,
    dist_features = dist_feat_tbl,
    cost = "cost"
  ) |>
    add_actions(actions_df, cost = 0) |>
    add_effects(effects_df) |>
    add_constraint_targets_relative(0.2) |>
    add_objective_min_cost() |>
    set_solver_cbc(time_limit = 10)

  sol <- solve(p)

  v <- get_solution_vector(sol)
  v
  length(v)
}
#> [1] 13
# }
```
