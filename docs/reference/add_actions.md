# Add management actions to a planning problem

Define the management action catalogue, the set of planning unit–action
pairs for which each action is feasible, and the corresponding
implementation costs.

This function adds two core components to a `Problem` object. First, it
stores the action catalogue in `x$data$actions`. Second, it builds the
feasible distribution of planning unit–action pairs in
`x$data$dist_actions`, including implementation costs, status codes, and
internal integer indices used by the optimization backend.

Conceptually, if \\\mathcal{P}\\ is the set of planning units and
\\\mathcal{A}\\ is the set of actions, this function defines a feasible
set \\\mathcal{F} \subseteq \mathcal{P} \times \mathcal{A}\\ together
with a non-negative cost function \\c : \mathcal{F} \to \mathbb{R}\_{\ge
0}\\.

## Usage

``` r
add_actions(
  x,
  actions,
  include = NULL,
  exclude = NULL,
  cost = NULL,
  feasible_default = TRUE,
  na_is_infeasible = TRUE
)
```

## Arguments

- x:

  A `Problem` object created with
  [`inputData`](https://josesalgr.github.io/mosap/reference/inputData.md).

- actions:

  A `data.frame` defining the action catalogue. It must contain a unique
  `id` column. A column named `action` is also accepted and
  automatically renamed to `id`.

- include:

  Optional feasibility specification defining which `(pu, action)` pairs
  are allowed. It can be `NULL`, a `data.frame` with columns `pu` and
  `action` (optionally also `feasible`), or a named list whose names are
  action ids and whose elements are vectors of planning unit ids or `sf`
  objects.

- exclude:

  Optional infeasibility specification. It uses the same formats as
  `include` and removes matching `(pu, action)` pairs from the feasible
  set.

- cost:

  Optional cost specification for feasible pairs. It may be `NULL`, a
  scalar numeric value, a named numeric vector indexed by action id, or
  a `data.frame` with columns `action, cost` or `pu, action, cost`.

- feasible_default:

  Logical. If `TRUE` and `include` is `NULL`, all possible
  `(pu, action)` pairs are considered feasible.

- na_is_infeasible:

  Logical. Relevant only when `include` or `exclude` is provided as a
  `data.frame` with a `feasible` column. If `TRUE`, missing values in
  `feasible` are treated as `FALSE`.

## Value

An updated `Problem` object with:

- `x$data$actions`:

  The action catalogue, including a unique integer `internal_id` for
  each action.

- `x$data$dist_actions`:

  The feasible planning unit–action distribution with columns `pu`,
  `action`, `cost`, `status`, `internal_pu`, and `internal_action`.

- `x$data$index$pu`:

  A mapping from user-supplied planning unit ids to internal integer
  ids.

- `x$data$index$action`:

  A mapping from action ids to internal integer ids.

## Details

The `actions` argument must be a `data.frame` with a unique `id` column
identifying each action. If a column named `action` is supplied instead,
it is renamed internally to `id`. Additional columns are preserved. If
no `name` column is provided, action names are taken from `id`. If an
`action_set` column is present, it is retained and can be used later for
grouped constraints or reporting.

Feasibility is controlled through `include` and `exclude`. If
`include = NULL` and `feasible_default = TRUE`, all possible
`(pu, action)` pairs are considered feasible. If `include` is supplied,
only those pairs are retained. If `exclude` is also supplied, the
specified pairs are removed from the feasible set after applying
`include`.

Both `include` and `exclude` may be specified as `NULL`, as a
`data.frame`, or as a named list. When provided as a `data.frame`, the
object must contain columns `pu` and `action`; an optional logical-like
column `feasible` can also be used, in which case rows with
`feasible = FALSE` are ignored. When provided as a named list, names
must match action identifiers. Each element of the list may contain
either a vector of planning unit ids or, when spatial planning units are
available in `x$data$pu_sf`, an `sf` layer defining the geographic area
where that action is feasible. In the latter case, feasible planning
units are identified using
[`sf::st_intersects()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.html).

It is important to distinguish feasibility from decision fixing. This
function only determines whether a `(pu, action)` pair is available to
the model. It does not force an action to be selected or prevented
beyond structural infeasibility. Fixed decisions should be imposed later
using
[`add_locked_actions`](https://josesalgr.github.io/mosap/reference/add_locked_actions.md).

Costs can be provided in several ways. If `cost` is `NULL`, all feasible
pairs receive a default cost of 1. If `cost` is a scalar, the same cost
is assigned to every feasible pair. If `cost` is a named numeric vector,
names must match action ids and costs are assigned by action. If `cost`
is a `data.frame`, it must define either action-level costs through
columns `action` and `cost`, or pair-specific costs through columns
`pu`, `action`, and `cost`. In all cases, costs must be finite and
non-negative.

Internally, all feasible pairs are initialized with `status = 0`,
indicating a free decision. If `x$data$pu$locked_out` exists and a
planning unit is marked as locked out, all actions in that planning unit
are assigned `status = 3`. This preserves consistency with planning-unit
exclusions already encoded in the problem object.

Calling `add_actions()` replaces any previous action catalogue and
feasible action distribution stored in the problem.

## See also

[`inputData`](https://josesalgr.github.io/mosap/reference/inputData.md),
[`add_locked_actions`](https://josesalgr.github.io/mosap/reference/add_locked_actions.md)

## Examples

``` r
# Minimal planning problem
pu <- data.frame(
  id = 1:4,
  cost = c(2, 3, 1, 4)
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

actions <- data.frame(
  id = c("conservation", "restoration"),
  name = c("Conservation", "Restoration")
)

# Example 1: all actions feasible everywhere, with action-level costs
p1 <- add_actions(
  x = p,
  actions = actions,
  cost = c(conservation = 5, restoration = 12)
)
#> Error: object 'p' not found

head(p1$data$dist_actions)
#> Error: object 'p1' not found

# Example 2: specify feasibility explicitly
include_df <- data.frame(
  pu = c(1, 2, 3, 4),
  action = c("conservation", "conservation", "restoration", "restoration")
)

p2 <- add_actions(
  x = p,
  actions = actions,
  include = include_df,
  cost = 10
)
#> Error: object 'p' not found

p2$data$dist_actions
#> Error: object 'p2' not found

# Example 3: exclude selected pairs after full expansion
exclude_df <- data.frame(
  pu = c(2, 4),
  action = c("restoration", "conservation")
)

p3 <- add_actions(
  x = p,
  actions = actions,
  exclude = exclude_df,
  cost = c(conservation = 3, restoration = 8)
)
#> Error: object 'p' not found

p3$data$dist_actions
#> Error: object 'p3' not found
```
