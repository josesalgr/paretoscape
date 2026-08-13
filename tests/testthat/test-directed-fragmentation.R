test_that("directed PU fragmentation uses origin-selected dependency coefficients", {
  p <- make_round3_tabular_problem()
  rel <- data.frame(pu1 = c(1, 2), pu2 = c(2, 1), weight = c(10, 5))
  p <- multiscape::add_spatial_relations(p, rel, name = "arcs", directed = TRUE)
  p <- multiscape::add_objective_min_fragmentation_planning_units(p, relation_name = "arcs")
  built <- multiscape:::.pa_build_model(p)
  ml <- built$data$model_list
  obj <- as.numeric(ml$obj)
  w0 <- as.integer(ml$w_offset)
  y0 <- as.integer(ml$y_pu_offset)
  expect_equal(obj[w0 + 1L], 10)
  expect_equal(obj[w0 + 2L], 5)
  expect_equal(obj[y0 + 1L], -15)
})

test_that("undirected PU fragmentation retains cut coefficients", {
  p <- make_round3_tabular_problem()
  p <- multiscape::add_spatial_relations(
    p, data.frame(pu1 = 1, pu2 = 2, weight = 10), name = "edge", directed = FALSE
  )
  p <- multiscape::add_objective_min_fragmentation_planning_units(p, relation_name = "edge")
  built <- multiscape:::.pa_build_model(p)
  ml <- built$data$model_list
  obj <- as.numeric(ml$obj)
  w0 <- as.integer(ml$w_offset)
  y0 <- as.integer(ml$y_pu_offset)
  expect_equal(obj[w0 + 1L], 10)
  expect_equal(obj[w0 + 2L], 10)
  expect_equal(obj[y0 + 1L], -20)
})

test_that("directed action fragmentation uses the same action at destination", {
  p <- make_round3_action_problem()
  p <- multiscape::add_spatial_relations(
    p, data.frame(pu1 = 1, pu2 = 2, weight = 7), name = "arc", directed = TRUE
  )
  p <- multiscape::add_objective_min_fragmentation_action(p, relation_name = "arc")
  built <- multiscape:::.pa_build_model(p)
  ml <- built$data$model_list
  obj <- as.numeric(ml$obj)
  da <- built$data$dist_actions_model
  x0 <- as.integer(ml$x_offset)
  y0 <- as.integer(ml$y_action_offset)
  pu1_rows <- da$internal_row[da$internal_pu == 1L]
  pu2_rows <- da$internal_row[da$internal_pu == 2L]
  expect_true(all(obj[x0 + pu1_rows] == 7))
  expect_true(all(obj[x0 + pu2_rows] == 0))
  expect_true(all(obj[y0 + seq_len(nrow(built$data$actions))] == -7))
})
