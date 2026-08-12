test_that("automatic epsilon values run from restrictive to permissive", {
  expect_equal(
    multiscape:::.pamo_order_epsilon_values(10, 50, 5, "min"),
    c(10, 20, 30, 40, 50)
  )
  expect_equal(
    multiscape:::.pamo_order_epsilon_values(10, 50, 5, "max"),
    c(50, 40, 30, 20, 10)
  )
})


test_that("Gurobi MIP starts are validated and normalized safely", {
  model <- list(
    obj = numeric(3),
    lb = c(0, 0, -1),
    ub = c(1, 10, 1),
    vtype = c("B", "I", "C")
  )

  out <- multiscape:::.pa_prepare_gurobi_mip_start(
    model,
    c(0.9999999, 4.000001, 2)
  )
  expect_true(out$applied)
  expect_identical(out$message, "applied")
  expect_equal(out$start, c(1, 4, 1))

  bad_length <- multiscape:::.pa_prepare_gurobi_mip_start(model, c(0, 1))
  expect_false(bad_length$applied)
  expect_match(bad_length$message, "length_mismatch")

  non_finite <- multiscape:::.pa_prepare_gurobi_mip_start(
    model,
    c(0, NA_real_, 0)
  )
  expect_false(non_finite$applied)
  expect_identical(non_finite$message, "ignored_non_finite")
})


test_that("solution vectors are recovered for sequential warm starts", {
  fake <- list(
    solution = list(
      solution = list(vector = c(0, 1, 0.5))
    )
  )
  expect_equal(multiscape:::.pamo_solution_vector(fake), c(0, 1, 0.5))
  expect_null(multiscape:::.pamo_solution_vector(list(solution = NULL)))
})


test_that("epsilon warm starts are enabled by default and can be disabled", {
  x <- make_explicit_mo_problem(n = 3, method = "epsilon")
  expect_true(x$data$method$warm_start)

  x_off <- multiscape::set_method_epsilon_constraint(
    x,
    primary = "cost",
    aliases = c("cost", "benefit"),
    runs = multiscape::set_runs_grid(n = 3),
    warm_start = FALSE
  )
  expect_false(x_off$data$method$warm_start)

  expect_error(
    multiscape::set_method_epsilon_constraint(
      x,
      primary = "cost",
      aliases = c("cost", "benefit"),
      runs = multiscape::set_runs_grid(n = 3),
      warm_start = NA
    ),
    "warm_start"
  )
})
