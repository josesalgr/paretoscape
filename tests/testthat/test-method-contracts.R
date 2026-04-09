test_that("set_method_weighted_sum errors when aliases and weights have different lengths", {
  toy <- toy_equivalent_basic()

  p <- multiscape::create_problem(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    multiscape::add_spatial_boundary(
      boundary = toy$boundary,
      weight_col = "boundary",
      include_self = TRUE
    ) |>
    multiscape::add_objective_min_cost(alias = "cost") |>
    multiscape::add_objective_min_fragmentation_pu(alias = "frag")

  expect_error(
    multiscape::set_method_weighted_sum(
      p,
      aliases = c("cost", "frag"),
      weights = c(1)
    ),
    "length"
  )
})

test_that("set_method_weighted_sum errors when aliases contain duplicates", {
  toy <- toy_equivalent_basic()

  p <- multiscape::create_problem(
    pu = toy$pu,
    features = toy$features,
    dist_features = toy$dist_features,
    cost = "cost"
  ) |>
    multiscape::add_spatial_boundary(
      boundary = toy$boundary,
      weight_col = "boundary",
      include_self = TRUE
    ) |>
    multiscape::add_objective_min_cost(alias = "cost") |>
    multiscape::add_objective_min_fragmentation_pu(alias = "frag")

  expect_error(
    multiscape::set_method_weighted_sum(
      p,
      aliases = c("cost", "cost"),
      weights = c(1, 1)
    )
  )
})
