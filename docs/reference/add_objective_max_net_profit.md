# Add objective: maximize net profit

Define an objective that maximizes net profit by combining profits with
optional planning-unit and action-cost penalties.

## Usage

``` r
add_objective_max_net_profit(
  x,
  profit_col = "profit",
  include_pu_cost = TRUE,
  include_action_cost = TRUE,
  actions = NULL,
  alias = NULL
)
```

## Arguments

- x:

  A `Problem` object.

- profit_col:

  Character string giving the profit column in the stored profit table.

- include_pu_cost:

  Logical. If `TRUE`, subtract planning-unit costs.

- include_action_cost:

  Logical. If `TRUE`, subtract action costs.

- actions:

  Optional subset of actions to include in the profit and action-cost
  terms. Values may match `x$data$actions$id` and, if present,
  `x$data$actions$action_set`.

- alias:

  Optional identifier used to register this objective for
  multi-objective workflows.

## Value

An updated `Problem` object.

## Details

Use this function when decisions generate returns and the objective
should optimize the resulting net balance after subtracting selected
cost components.

Let:

- \\x\_{ia} \in \\0,1\\\\ denote whether action \\a\\ is selected in
  planning unit \\i\\,

- \\w_i \in \\0,1\\\\ denote whether planning unit \\i\\ is selected,

- \\\pi\_{ia}\\ denote the profit associated with decision \\(i,a)\\,

- \\c_i^{PU} \ge 0\\ denote the planning-unit cost,

- \\c\_{ia}^{A} \ge 0\\ denote the action cost.

In its most general form, the objective is:

\$\$ \max \left( \sum\_{(i,a) \in \mathcal{D}^{\star}} \pi\_{ia}
x\_{ia} - \sum\_{i \in \mathcal{I}} c_i^{PU} w_i - \sum\_{(i,a) \in
\mathcal{D}^{\star}} c\_{ia}^{A} x\_{ia} \right), \$\$

where \\\mathcal{D}^{\star}\\ denotes the subset of feasible planning
unit–action decisions included in the objective.

If `actions = NULL`, all feasible actions contribute to both the profit
term and the action-cost term.

If `actions` is provided, the profit term and the action-cost term are
restricted to that subset. The planning-unit cost term, if included,
remains global.

If `include_pu_cost = FALSE`, the planning-unit cost term is omitted.

If `include_action_cost = FALSE`, the action-cost term is omitted.

## See also

[`add_objective_max_profit`](https://josesalgr.github.io/multiscape/reference/add_objective_max_profit.md),
[`add_objective_min_cost`](https://josesalgr.github.io/multiscape/reference/add_objective_min_cost.md)

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

p1 <- add_objective_max_net_profit(p)
p1$data$model_args
#> $model_type
#> [1] "maximizeNetProfit"
#> 
#> $objective_id
#> [1] "max_net_profit"
#> 
#> $objective_args
#> $objective_args$profit_col
#> [1] "profit"
#> 
#> $objective_args$include_pu_cost
#> [1] TRUE
#> 
#> $objective_args$include_action_cost
#> [1] TRUE
#> 
#> $objective_args$actions
#> NULL
#> 
#> 

p2 <- add_objective_max_net_profit(
  p,
  include_pu_cost = FALSE,
  include_action_cost = TRUE
)
p2$data$model_args
#> $model_type
#> [1] "maximizeNetProfit"
#> 
#> $objective_id
#> [1] "max_net_profit"
#> 
#> $objective_args
#> $objective_args$profit_col
#> [1] "profit"
#> 
#> $objective_args$include_pu_cost
#> [1] FALSE
#> 
#> $objective_args$include_action_cost
#> [1] TRUE
#> 
#> $objective_args$actions
#> NULL
#> 
#> 

p3 <- add_objective_max_net_profit(
  p,
  actions = "restoration"
)
p3$data$model_args
#> $model_type
#> [1] "maximizeNetProfit"
#> 
#> $objective_id
#> [1] "max_net_profit"
#> 
#> $objective_args
#> $objective_args$profit_col
#> [1] "profit"
#> 
#> $objective_args$include_pu_cost
#> [1] TRUE
#> 
#> $objective_args$include_action_cost
#> [1] TRUE
#> 
#> $objective_args$actions
#> [1] "restoration"
#> 
#> 
```
