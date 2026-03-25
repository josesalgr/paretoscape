# Add absolute targets

Add feature-level absolute targets to a planning problem.

These targets are stored in `x$data$targets` and later translated into
linear constraints when the optimization model is built.

Absolute targets are interpreted directly in the same units as the
feature contributions used by the model.

## Usage

``` r
add_targets_absolute(
  x,
  targets,
  subset = NULL,
  overwrite = FALSE,
  label = NULL
)
```

## Arguments

- x:

  A `Problem` object.

- targets:

  Target specification. This is interpreted as an absolute target value
  in the same units as the modelled feature contributions. See Details.

- subset:

  Optional character vector indicating which actions count toward target
  achievement. Entries may match action ids, `action_set` labels, or
  both. If `NULL`, all actions count.

- overwrite:

  Logical. If `TRUE`, replace existing stored targets for the same
  feature and subset combination. If `FALSE`, new rows are appended and
  overlap handling is delegated to `.pa_store_targets()`.

- label:

  Optional character string stored with the targets for reporting and
  bookkeeping.

## Value

An updated `Problem` object with absolute targets stored in
`x$data$targets`.

## Details

Let \\\mathcal{F}\\ denote the set of features. For each feature \\f \in
\mathcal{F}\\, this function stores an absolute target threshold \\T_f
\ge 0\\.

When the optimization model is built, each such target is interpreted as
a lower-bound constraint of the form: \$\$ \sum\_{(i,a) \in
\mathcal{S}\_f} c\_{iaf} x\_{ia} \ge T_f, \$\$ where:

- \\x\_{ia}\\ indicates whether action \\a\\ is selected in planning
  unit \\i\\,

- \\c\_{iaf}\\ is the contribution of that action to feature \\f\\,

- \\\mathcal{S}\_f\\ is the set of planning unit–action pairs allowed to
  count toward the target.

In the absolute case, the stored target threshold is simply: \$\$ T_f =
t_f, \$\$ where \\t_f\\ is the user-supplied target value for feature
\\f\\.

The `subset` argument restricts which actions may contribute toward
achievement of the target, but it does not modify the value of \\T_f\\
itself.

The `targets` argument is parsed by `.pa_parse_targets()` and may be
supplied in several equivalent forms, including:

- a single numeric value recycled to all features,

- a numeric vector aligned to feature order,

- a named numeric vector where names identify features,

- a `data.frame` with `feature` and `target` columns.

Internally, targets added by this function are stored with:

- `type = "actions"`,

- `sense = "ge"`,

- `target_unit = "absolute"`,

- `target_raw = target_value`,

- `basis_total = NA`.

## See also

[`add_targets_relative`](https://josesalgr.github.io/mosap/reference/add_targets_relative.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Same absolute target for all features
p <- add_targets_absolute(p, 10)

# Different targets by feature id
p <- add_targets_absolute(
  p,
  c("1" = 5, "2" = 8, "3" = 12)
)

# Only actions in the "recovery" set count toward target achievement
p <- add_targets_absolute(
  p,
  5,
  subset = "recovery"
)

# Mix action sets and specific actions
p <- add_targets_absolute(
  p,
  5,
  subset = c("recovery", "conservation_action")
)
} # }
```
