# Add objective: maximize profit

Define an objective that maximizes total profit from selected planning
unit–action decisions.

## Usage

``` r
add_objective_max_profit(
  x,
  profit_col = "profit",
  actions = NULL,
  alias = NULL
)
```

## Arguments

- x:

  A `Problem` object.

- profit_col:

  Character string giving the profit column in the stored profit table.

- actions:

  Optional subset of actions to include. Values may match
  `x$data$actions$id` and, if present, `x$data$actions$action_set`. If
  `NULL`, all actions are included.

- alias:

  Optional identifier used to register this objective for
  multi-objective workflows.

## Value

An updated `Problem` object.

## Details

Use this function when the objective is to maximize gross economic
return, without subtracting planning-unit or action costs.

Let \\x\_{ia} \in \\0,1\\\\ denote whether action \\a\\ is selected in
planning unit \\i\\, and let \\\pi\_{ia}\\ denote the profit associated
with that decision, as taken from column `profit_col` in the stored
profit table.

If all actions are included, the objective is:

\$\$ \max \sum\_{(i,a) \in \mathcal{D}} \pi\_{ia} x\_{ia}, \$\$

where \\\mathcal{D}\\ denotes the set of feasible planning unit–action
decisions.

If `actions` is provided, only the selected subset contributes to the
objective. Letting \\\mathcal{D}^{\star}\\ denote the feasible decisions
whose action belongs to the selected subset, the objective becomes:

\$\$ \max \sum\_{(i,a) \in \mathcal{D}^{\star}} \pi\_{ia} x\_{ia}. \$\$

This objective considers profit only. It does not subtract planning-unit
costs or action costs. For a net-profit formulation, use
[`add_objective_max_net_profit`](https://josesalgr.github.io/multiscape/reference/add_objective_max_net_profit.md).

## See also

[`add_objective_min_cost`](https://josesalgr.github.io/multiscape/reference/add_objective_min_cost.md),
[`add_objective_max_net_profit`](https://josesalgr.github.io/multiscape/reference/add_objective_max_net_profit.md)

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
profit_df <- data.frame(
  pu = c(1, 2, 3, 4, 1, 2, 3, 4),
  action = c("conservation", "conservation", "conservation", "conservation",
             "restoration", "restoration", "restoration", "restoration"),
  profit = c(5, 4, 3, 2, 8, 7, 6, 5)
)

p <- create_problem(
  pu = pu_tbl,
  features = feat_tbl,
  dist_features = dist_feat_tbl,
  cost = "cost"
) |>
  add_actions(actions_df, cost = c(conservation = 1, restoration = 2)) |>
  add_profit(profit_df)

p1 <- add_objective_max_profit(p)
p1$data$model_args
#> $model_type
#> [1] "maximizeProfit"
#> 
#> $objective_id
#> [1] "max_profit"
#> 
#> $objective_args
#> $objective_args$profit_col
#> [1] "profit"
#> 
#> $objective_args$actions
#> NULL
#> 
#> 

p2 <- add_objective_max_profit(
  p,
  actions = "restoration"
)
p2$data$model_args
#> $model_type
#> [1] "maximizeProfit"
#> 
#> $objective_id
#> [1] "max_profit"
#> 
#> $objective_args
#> $objective_args$profit_col
#> [1] "profit"
#> 
#> $objective_args$actions
#> [1] "restoration"
#> 
#> 
```
