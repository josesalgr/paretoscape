test_that("epsilon-constraint returns a SolutionSet with runs", {
  skip_if_no_cbc()

  toy <- toy_equivalent_basic()

  p <- multiscape::input_data(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    multiscape::add_actions(actions = toy$actions, cost = 0) |>
    multiscape::add_effects(effects = toy$effects, effect_type = "after") |>
    multiscape::add_targets_relative(0.5) |>
    multiscape::add_objective_min_cost(alias = "cost") |>
    multiscape::add_objective_max_benefit(alias = "benefit") |>
    multiscape::set_method_epsilon_constraint(
      primary = "cost",
      aliases = c("cost", "benefit"),
      mode = "manual",
      eps = data.frame(
        benefit = c(0, 1)
      )
    ) |>
    multiscape::set_solver_cbc(gap_limit = 0, verbose = FALSE)

  s <- multiscape::solve(p)

  expect_s3_class(s, "SolutionSet")
  expect_true(is.list(s$solution))
  expect_true("runs" %in% names(s$solution))
  expect_true("solutions" %in% names(s$solution))
  expect_gte(nrow(s$solution$runs), 1)
  expect_gte(length(s$solution$solutions), 1)
})
