# Add objective: minimize loss

Define an objective that minimizes the total negative effects generated
by selected actions on selected features.

This objective is based on the canonical effects table and uses only the
non-negative `loss` component.

## Usage

``` r
add_objective_min_loss(x, actions = NULL, features = NULL, alias = NULL)
```

## Arguments

- x:

  A `Problem` object.

- actions:

  Optional subset of actions to include in the objective. Values may
  match `x$data$actions$id` and, if present,
  `x$data$actions$action_set`. If `NULL`, all actions are included.

- features:

  Optional subset of features to include in the objective. Values may
  match `x$data$features$id` and, if present, `x$data$features$name`. If
  `NULL`, all features are included.

- alias:

  Optional identifier used to register this objective for
  multi-objective workflows.

## Value

An updated `Problem` object.

## Details

Use this function when harmful ecological effects should be minimized
explicitly, without offsetting them against beneficial effects.

Let \\\ell\_{iaf} \ge 0\\ denote the stored loss associated with
planning unit \\i\\, action \\a\\, and feature \\f\\.

If no subsets are supplied, the objective can be written as:

\$\$ \min \sum\_{(i,a,f) \in \mathcal{R}} \ell\_{iaf} \\ x\_{ia}. \$\$

where \\\mathcal{R}\\ denotes the set of stored loss rows and \\x\_{ia}
\in \\0,1\\\\ indicates whether action \\a\\ is selected in planning
unit \\i\\.

If `actions` is provided, only rows whose action belongs to the selected
subset contribute to the objective.

If `features` is provided, only rows whose feature belongs to the
selected subset contribute to the objective.

More generally, letting \\\mathcal{R}^{\star}\\ be the subset induced by
the selected actions and features, the objective is:

\$\$ \min \sum\_{(i,a,f) \in \mathcal{R}^{\star}} \ell\_{iaf} \\
x\_{ia}. \$\$

This objective minimizes harmful effects only. It does not offset losses
against benefits unless benefits are handled elsewhere through
additional objectives or constraints.

## See also

[`add_objective_max_benefit`](https://josesalgr.github.io/multiscape/reference/add_objective_max_benefit.md),
[`add_effects`](https://josesalgr.github.io/multiscape/reference/add_effects.md)

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
actions_df <- data.frame(
  id = c("conservation", "restoration"),
  name = c("conservation", "restoration")
)
effects_df <- data.frame(
  pu = c(1, 2, 3, 4, 1, 2, 3, 4),
  action = c("conservation", "conservation", "conservation", "conservation",
             "restoration", "restoration", "restoration", "restoration"),
  feature = c(1, 1, 1, 1, 2, 2, 2, 2),
  benefit = c(2, 1, 0, 1, 3, 0, 1, 2),
  loss = c(0, 0, 1, 0, 0, 1, 0, 0)
)

p <- create_problem(
  pu = pu_tbl,
  features = feat_tbl,
  dist_features = dist_feat_tbl,
  cost = "cost"
) |>
  add_actions(actions_df, cost = c(conservation = 1, restoration = 2)) |>
  add_effects(effects_df)

p1 <- add_objective_min_loss(p)
p1$data$model_args
#> $model_type
#> [1] "minimizeLosses"
#> 
#> $objective_id
#> [1] "min_loss"
#> 
#> $objective_args
#> $objective_args$actions
#> NULL
#> 
#> $objective_args$features
#> NULL
#> 
#> 

p2 <- add_objective_min_loss(
  p,
  actions = "restoration"
)
p2$data$model_args
#> $model_type
#> [1] "minimizeLosses"
#> 
#> $objective_id
#> [1] "min_loss"
#> 
#> $objective_args
#> $objective_args$actions
#> [1] 2
#> 
#> $objective_args$features
#> NULL
#> 
#> 

p3 <- add_objective_min_loss(
  p,
  features = 1
)
p3$data$model_args
#> $model_type
#> [1] "minimizeLosses"
#> 
#> $objective_id
#> [1] "min_loss"
#> 
#> $objective_args
#> $objective_args$actions
#> NULL
#> 
#> $objective_args$features
#> [1] 1
#> 
#> 
```
