# Set the weighted-sum multi-objective method

Configure a `Problem` object to be solved with a weighted-sum
multi-objective method.

In the weighted-sum method, several registered atomic objectives are
combined into a single scalar objective using a weighted linear
combination. This function stores that configuration in `x$data$method`
so that it can be used later by
[`solve`](https://josesalgr.github.io/multiscape/reference/solve.md).

## Usage

``` r
set_method_weighted_sum(
  x,
  aliases,
  weights,
  normalize_weights = FALSE,
  objective_scaling = FALSE
)
```

## Arguments

- x:

  A `Problem` object.

- aliases:

  Character vector of objective aliases to combine. Each alias must
  correspond to a previously registered atomic objective.

- weights:

  Numeric vector of weights, with the same length and order as
  `aliases`. Weights must be finite. If `normalize_weights = TRUE`, they
  are rescaled to sum to 1 before being stored.

- normalize_weights:

  Logical. If `TRUE`, normalize the supplied weights to sum to 1 before
  storing them.

- objective_scaling:

  Logical. If `TRUE`, request scaling of the participating objectives
  before weighted aggregation in the solving layer.

## Value

The updated `Problem` object with the weighted-sum method configuration
stored in `x$data$method`.

## Details

Use this method when several registered objectives should be combined
into a single scalar optimization problem through explicit preference
weights.

**General idea**

Suppose that a set of atomic objectives has already been registered in
the problem under aliases \\k \in \mathcal{K}\\. Let \\f_k(x)\\ denote
the scalar value of objective \\k\\, and let \\w_k\\ denote its
user-supplied weight.

The weighted-sum method combines them into a single scalar objective of
the form:

\$\$ \sum\_{k \in \mathcal{K}} w_k \\ f_k(x). \$\$

In practice, the exact sign convention used internally depends on the
sense of each registered atomic objective, for example whether it is a
minimization-type or maximization-type objective. The solving layer is
therefore responsible for constructing a solver-ready scalar objective
from the stored objective specifications and the requested weights.

**Atomic objectives requirement**

The weighted-sum method can only be used with atomic objectives that
have already been registered under aliases. These aliases are typically
created by calling objective setters with an `alias` argument, for
example:


    x <- x |>
      add_objective_min_cost(alias = "cost") |>
      add_objective_min_fragmentation(alias = "frag")

Internally, each atomic objective is stored in
`x$data$objectives[[alias]]` together with its metadata, such as:

- `objective_id`,

- `model_type`,

- `sense`,

- `objective_args`.

The `aliases` argument passed to this function selects which of those
registered atomic objectives are included in the weighted combination.

**Weight normalization**

If `normalize_weights = TRUE`, the supplied weights are rescaled to sum
to 1:

\$\$ \tilde{w}\_k = \frac{w_k}{\sum\_{j \in \mathcal{K}} w_j}. \$\$

This normalization does not change the optimizer's solution in a pure
weighted-sum formulation as long as all weights are multiplied by the
same positive constant, but it can improve interpretability and
numerical conditioning.

If `normalize_weights = FALSE`, the supplied weights are stored exactly
as provided.

**Objective scaling**

If `objective_scaling = TRUE`, the solving layer is expected to scale
the participating objectives before combining them. The purpose of
scaling is to reduce distortions caused by objectives being measured on
very different numerical ranges.

Conceptually, if \\R_k\\ denotes a scale or range associated with
objective \\k\\, then a scaled weighted sum may be interpreted as:

\$\$ \sum\_{k \in \mathcal{K}} w_k \\ \frac{f_k(x)}{R_k}. \$\$

The exact scaling rule is implemented later in the solving layer. This
function only stores whether objective scaling has been requested.

**Mixed objective senses**

Weighted sums are straightforward when all participating objectives have
the same optimization sense. When minimization and maximization
objectives are mixed, the solving layer must standardize them internally
before building the scalar objective.

This function validates that the requested aliases exist, but it does
not itself resolve objective-sense compatibility. That logic is
delegated to the downstream solving layer.

**Theoretical limitation**

The weighted-sum method typically recovers only *supported* efficient
solutions, that is, solutions lying on the convex hull of the Pareto
front in objective space. In non-convex multi-objective problems,
especially mixed integer problems, some efficient solutions cannot be
obtained by any weighted combination. In such cases, methods such as
[`set_method_epsilon_constraint`](https://josesalgr.github.io/multiscape/reference/set_method_epsilon_constraint.md)
or
[`set_method_augmecon`](https://josesalgr.github.io/multiscape/reference/set_method_augmecon.md)
may be preferable.

**Stored configuration**

This function stores the method definition in `x$data$method` with:

- `name = "weighted"`,

- `aliases`,

- `weights`,

- `normalize_weights`,

- `objective_scaling`.

The actual scalarization is performed later by
[`solve`](https://josesalgr.github.io/multiscape/reference/solve.md).

## See also

[`set_method_epsilon_constraint`](https://josesalgr.github.io/multiscape/reference/set_method_epsilon_constraint.md),
[`set_method_augmecon`](https://josesalgr.github.io/multiscape/reference/set_method_augmecon.md),
[`solve`](https://josesalgr.github.io/multiscape/reference/solve.md)

## Examples

``` r
# Small toy problem
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

x <- create_problem(
  pu = pu_tbl,
  features = feat_tbl,
  dist_features = dist_feat_tbl,
  cost = "cost"
) |>
  add_actions(actions_df, cost = c(conservation = 1, restoration = 2)) |>
  add_effects(effects_df) |>
  add_objective_min_cost(alias = "cost") |>
  add_objective_max_benefit(alias = "benefit")

x <- set_method_weighted_sum(
  x,
  aliases = c("cost", "benefit"),
  weights = c(0.4, 0.6),
  normalize_weights = FALSE
)

x$data$method
#> $name
#> [1] "weighted"
#> 
#> $aliases
#> [1] "cost"    "benefit"
#> 
#> $weights
#> [1] 0.4 0.6
#> 
#> $normalize_weights
#> [1] FALSE
#> 
#> $objective_scaling
#> [1] FALSE
#> 

# Normalize weights before storing
x2 <- set_method_weighted_sum(
  x,
  aliases = c("cost", "benefit"),
  weights = c(2, 3),
  normalize_weights = TRUE
)

x2$data$method
#> $name
#> [1] "weighted"
#> 
#> $aliases
#> [1] "cost"    "benefit"
#> 
#> $weights
#> [1] 0.4 0.6
#> 
#> $normalize_weights
#> [1] TRUE
#> 
#> $objective_scaling
#> [1] FALSE
#> 

# Request objective scaling
x3 <- set_method_weighted_sum(
  x,
  aliases = c("cost", "benefit"),
  weights = c(0.7, 0.3),
  objective_scaling = TRUE
)

x3$data$method
#> $name
#> [1] "weighted"
#> 
#> $aliases
#> [1] "cost"    "benefit"
#> 
#> $weights
#> [1] 0.7 0.3
#> 
#> $normalize_weights
#> [1] FALSE
#> 
#> $objective_scaling
#> [1] TRUE
#> 
```
