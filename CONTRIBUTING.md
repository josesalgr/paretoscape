Contributing to multiscape
================

`multiscape` is being developed as a modular framework for exact
multi-objective spatial planning in R. Its development benefits from
collaboration across programming, mathematical optimisation, ecology,
conservation planning, and spatial analysis. Because robust
decision-support tools require sustained development and
interdisciplinary input, contributions from different backgrounds are
very welcome.

We welcome contributions to code, documentation, testing, examples, and
methodological development.

## Improving documentation

The package documentation and website are generated automatically from
function documentation, examples, articles, and markdown source files.
This means there is no need to edit HTML manually. If you are familiar
with package documentation workflows in R, you are welcome to propose
improvements by editing `.R`, `.Rmd`, or `.md` files and opening a pull
request.

Documentation improvements may include:

- fixing typos or unclear wording,
- improving function documentation,
- adding or refining examples,
- clarifying optimisation methods or modelling assumptions,
- improving articles, tutorials, or contribution guides.

If you are not sure how to implement a documentation change yourself,
feel free to open an
[issue](https://github.com/josesalgr/multiscape/issues).

## Reporting bugs

If you find a bug, please open an
[issue](https://github.com/josesalgr/multiscape/issues) in the GitHub
repository.

A useful bug report should include:

- a clear description of the problem,
- the function or workflow involved,
- a minimal reproducible example whenever possible,
- the expected behaviour,
- the observed behaviour,
- and session information if relevant.

Minimal reproducible examples are especially helpful because they make
it much easier to diagnose and fix the problem.

## Asking questions

If you are using `multiscape` and get stuck, first check the function
documentation, examples, and available package materials. If your
question is still unresolved, you can open an
[issue](https://github.com/josesalgr/multiscape/issues) on GitHub. While
user support cannot always be guaranteed, questions often help improve
the documentation, reveal edge cases, or identify bugs.

## Proposing ideas and enhancements

If you have an idea for a new feature, modelling extension, optimisation
method, or usability improvement in `multiscape`, please first check
whether it has already been discussed in the issue tracker.

If not, feel free to open an
[issue](https://github.com/josesalgr/multiscape/issues) describing your
proposal. It is especially helpful if you explain:

- the motivation for the feature,
- the intended use case,
- how it would fit the current modular workflow,
- and whether it affects the public API, optimisation model, or result
  objects.

Well-scoped proposals are usually easier to evaluate and discuss than
very broad ones.

## Contributing code

Before making substantial changes to the R or C++ code, please make sure
that someone from the `multiscape` team agrees that the proposed change
is needed.

- Fork the repository and clone it to your computer. If you have not
  done this before, we recommend using
  `usethis::create_from_github("josesalgr/multiscape", fork = TRUE)`.

- Install the development dependencies with
  `devtools::install_dev_deps()`.

- Make sure the package passes `R CMD check`, for example with
  `devtools::check()`. If the package does not pass checks cleanly, it
  is a good idea to ask for help before continuing.

- Create a Git branch for your pull request. We recommend using
  `usethis::pr_init("brief-description-of-change")`.

- Make your changes, commit them, and then create a pull request with
  `usethis::pr_push()`, following the prompts in your browser.

- For user-facing changes, add a bullet point to the top of
  [NEWS.md](https://github.com/josesalgr/multiscape/blob/main/NEWS.md),
  just below the first header. Please follow the style described in the
  tidyverse news guidelines.

## Pull requests

When opening a pull request, please make sure that:

- the proposed change is focused and clearly described,
- the package still passes `devtools::check()`,
- new functionality includes tests when appropriate,
- user-facing changes include documentation updates,
- and the pull request body references the related issue when
  applicable.

If the change affects package behaviour from a user perspective, please
also add a short bullet point to
[NEWS.md](https://github.com/josesalgr/multiscape/blob/main/NEWS.md)
under the most recent development section.

## Style and scope

Please try to keep contributions consistent with the current design of
`multiscape`, especially its modular structure:

- `create_problem()` creates the core problem object,
- `add_*()` functions add actions, effects, objectives, constraints, or
  spatial relations,
- `set_method_*()` functions configure the multi-objective solution
  strategy,
- `set_solver()` configures the optimisation solver,
- `compile_model()` builds the optimisation model,
- `solve()` executes the optimisation workflow,
- `get_*()` and plotting functions expose and summarise results.

Changes that preserve and strengthen this modular structure are usually
preferred over monolithic shortcuts or tightly coupled additions.

## Testing contributions

Whenever possible, new functionality should include tests. Bug fixes
should ideally include a regression test that fails before the fix and
passes afterwards. If a contribution affects optimisation behaviour, it
is often useful to test both object construction and solution output
where feasible.

If your contribution depends on optional solver backends, please make
sure the code degrades gracefully when those backends are not available.

## Code style

Please try to keep the style of new code consistent with the existing
package. In particular:

- prefer clear and explicit function names,
- preserve the modular workflow and object structure,
- keep user-facing error messages informative,
- and update documentation when changing exported behaviour.

Small, focused pull requests are usually easier to review and merge than
large ones mixing several unrelated changes.

## Code of conduct

Please be respectful and constructive in issues, discussions, and pull
requests. Contributions are welcome from researchers and practitioners
with different backgrounds, and a collaborative tone helps keep the
project useful and sustainable.
