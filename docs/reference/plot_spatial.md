# Plot spatial outputs from a solution or solution set

Plot spatial outputs derived from a `Solution` or `SolutionSet` object.

This function provides three main spatial views:

- `"pu"`: selected planning units,

- `"actions"`: selected actions in space,

- `"features"`: spatial feature values based on baseline or
  action-induced changes.

The function requires planning-unit geometry to be available in
`x$problem$data$pu_sf`.

## Usage

``` r
plot_spatial(
  x,
  what = c("pu", "actions", "features"),
  runs = NULL,
  subset = NULL,
  value = c("final", "baseline", "benefit"),
  layout = NULL,
  max_facets = 4L,
  only_selected = FALSE,
  ...,
  base_alpha = 0.1,
  selected_alpha = 0.9,
  base_fill = "grey92",
  base_color = NA,
  selected_color = NA,
  draw_borders = FALSE,
  show_base = TRUE,
  fill_values = NULL,
  fill_na = "grey80",
  use_viridis = TRUE
)
```

## Arguments

- x:

  A `Solution` or `SolutionSet` object.

- what:

  Character string indicating what to plot. Must be one of `"pu"`,
  `"actions"`, or `"features"`.

- runs:

  Optional integer vector of run ids. If `NULL`, a `Solution` is used
  directly and a `SolutionSet` defaults to the first run.

- subset:

  Optional character vector used to restrict actions or features,
  depending on `what`.

- value:

  Character string used only when `what = "features"`. Must be one of
  `"final"`, `"baseline"`, or `"benefit"`.

- layout:

  Character string controlling the layout. Must be one of `"single"` or
  `"facet"`. If `NULL`, the default is `"single"` for `"pu"` and
  `"actions"`, and `"facet"` for `"features"`.

- max_facets:

  Maximum number of facets shown when `subset = NULL` and faceting would
  otherwise create many panels.

- only_selected:

  Logical. Used only when `what = "pu"`. If `TRUE`, highlight only
  selected planning units.

- ...:

  Reserved for future extensions.

- base_alpha:

  Numeric value in \\\[0,1\]\\ giving the alpha of the base
  planning-unit layer.

- selected_alpha:

  Numeric value in \\\[0,1\]\\ giving the alpha of the highlighted
  layer.

- base_fill:

  Fill colour for the base planning-unit layer.

- base_color:

  Border colour for the base planning-unit layer.

- selected_color:

  Border colour for highlighted layers.

- draw_borders:

  Logical. If `FALSE`, borders are not drawn.

- show_base:

  Logical. If `TRUE`, draw the base planning-unit layer underneath the
  highlighted output.

- fill_values:

  Optional named vector of colours for discrete maps.

- fill_na:

  Fill colour for missing values.

- use_viridis:

  Logical. If `TRUE` and the viridis package is available, use viridis
  scales.

## Value

Invisibly returns a `ggplot` object.

## Details

**General behaviour**

This function is a plotting helper built on top of the stored solution
summaries and the geometry preserved in the associated `Problem` object.
It does not rebuild a spatial problem from scratch. Instead, it
combines:

- geometry from `x$problem$data$pu_sf`,

- solution summaries extracted with
  [`get_pu`](https://josesalgr.github.io/mosap/reference/get_pu.md),
  [`get_actions`](https://josesalgr.github.io/mosap/reference/get_actions.md),
  and related internal data,

- baseline feature distributions from `x$problem$data$dist_features`,

- and, when needed, effect information from
  `x$problem$data$dist_effects` or `x$problem$data$dist_effects_model`.

The returned object is a `ggplot` object, which is also printed.

**Run handling**

If `x` is a `Solution`, then `runs` must be `NULL` or `1`.

If `x` is a `SolutionSet`, the `runs` argument selects which runs to
plot. If `runs = NULL`, the first run is used by default.

Some plotting modes impose additional restrictions when several runs are
displayed simultaneously:

- for `what = "actions"`, `layout = "facet"` is not allowed when
  plotting multiple runs,

- for `what = "features"`, plotting multiple runs requires that `subset`
  specify exactly one feature.

**View: `what = "pu"`**

This view plots planning units and highlights those with `selected == 1`
in the stored planning-unit summary.

Let \\w_i \in \\0,1\\\\ denote the planning-unit selection variable for
planning unit \\i\\. This view is the spatial representation of the
user-facing `selected` version of \\w_i\\.

If `only_selected = TRUE`, only selected planning units are shown as
highlighted output. If several runs are plotted, the panels are faceted
by run.

**View: `what = "actions"`**

This view plots selected planning unit–action decisions. It is based on
the action summary returned by
[`get_actions`](https://josesalgr.github.io/mosap/reference/get_actions.md)`(only_selected = TRUE)`.

Let \\x\_{ia} \in \\0,1\\\\ denote whether action \\a\\ is selected in
planning unit \\i\\. This view maps the selected `(pu, action)` pairs
onto planning-unit geometry.

If more than one action is selected in the same planning unit and
`layout = "single"`, action labels are collapsed using `"+"` and a
warning is emitted.

If `layout = "facet"` and only one run is plotted, one panel is drawn
per action. If `layout = "single"`, all selected actions are drawn in a
single map using discrete fill values.

The `subset` argument can be used to restrict the displayed actions.

**View: `what = "features"`**

This view plots feature values at the planning-unit level. It combines
the baseline feature distribution stored in `dist_features` with
positive action-induced effects from the selected solution.

For each planning unit \\i\\ and feature \\f\\, the plotted quantities
are: \$\$ \mathrm{baseline}\_{if}, \$\$ \$\$ \mathrm{benefit}\_{if},
\$\$ \$\$ \mathrm{final}\_{if} = \mathrm{baseline}\_{if} +
\mathrm{benefit}\_{if}. \$\$

In the current implementation, the `"features"` view uses:

- `baseline`: the summed baseline amount from
  `x$problem$data$dist_features`,

- `benefit`: the summed positive effect from selected actions only,

- `final`: `baseline + benefit`.

Negative effects are not subtracted in this plotting method. Therefore,
the `"final"` map should be interpreted as `baseline + selected benefit`
under the currently stored plotting logic, not necessarily as a full
net-effect reconstruction.

The `subset` argument can be used to restrict which features are shown.
Feature matching is attempted against both feature ids and feature
names.

If `layout = "facet"` and only one run is plotted, one panel is drawn
per feature. If multiple runs are plotted, faceting is done by run
instead.

**Base layer and drawing options**

If `show_base = TRUE`, the planning-unit geometry is first drawn as a
low-alpha background layer using `base_fill` and `base_color`. The
highlighted output is then drawn on top.

If `draw_borders = FALSE`, border colors are disabled to improve speed,
which is especially useful for large spatial datasets.

**Colour scales**

For discrete maps such as `"actions"`, colours may be supplied through
`fill_values`. Otherwise, discrete viridis colours are used when
available and `use_viridis = TRUE`.

For continuous maps such as `"features"`, a continuous viridis colour
scale is used when available and `use_viridis = TRUE`.

## See also

[`get_pu`](https://josesalgr.github.io/mosap/reference/get_pu.md),
[`get_actions`](https://josesalgr.github.io/mosap/reference/get_actions.md),
[`get_features`](https://josesalgr.github.io/mosap/reference/get_features.md),
[`solve`](https://josesalgr.github.io/mosap/reference/solve.md)
