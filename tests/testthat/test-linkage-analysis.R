test_that("get_solution_states returns one canonical state per PU and solution", {
  x <- make_mock_solutionset()

  expect_equal(names(formals(get_solution_states)), "x")

  states <- get_solution_states(x)

  expect_s3_class(states, "data.frame")
  expect_named(
    states,
    c("solution_id", "pu", "selected", "state", "managed", "n_actions")
  )
  expect_equal(nrow(states), 8L)
  expect_type(states$solution_id, "integer")
  expect_equal(sort(unique(states$solution_id)), c(1L, 2L))
  expect_equal(states$state[states$solution_id == 1L],
               c("conservation", "unmanaged", "unmanaged", "restoration"))
  expect_equal(states$selected, c(1L, 0L, 0L, 1L, 0L, 1L, 1L, 0L))
  expect_equal(sum(states$managed), 4L)
  expect_equal(sum(states$state == "unmanaged"), 4L)
  expect_false(any(states$state == "no_action"))
  expect_false(any(states$state == "none"))
  expect_error(
    get_solution_states(x, solution = 1L),
    "unused argument"
  )
  expect_error(get_solution_states(list()), "SolutionSet")
})


test_that("get_solution_states combines simultaneous actions deterministically", {
  x <- make_mock_solutionset()
  extra <- x$summary$actions[1L, , drop = FALSE]
  extra$internal_action <- 2L
  extra$action <- "restoration"
  extra$selected <- 1
  x$summary$actions <- rbind(x$summary$actions, extra)

  states <- get_solution_states(x)
  combined <- states[
    states$solution_id == 1L & as.character(states$pu) == "1",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(combined), 1L)
  expect_equal(combined$state, "conservation+restoration")
  expect_true(combined$managed)
  expect_equal(combined$n_actions, 2L)
})


test_that("get_solution_states composes with solution_filter", {
  x <- make_mock_solutionset()

  one_solution <- solution_filter(x, solution_id = 2L)
  states <- get_solution_states(one_solution)

  expect_type(states$solution_id, "integer")
  expect_equal(unique(states$solution_id), 2L)
  expect_equal(nrow(states), 4L)
})


test_that("get_solution_states rejects non-numeric stored ids", {
  x <- make_mock_solutionset()
  x$solution$runs$solution_id[1L] <- "A"

  expect_error(
    get_solution_states(x),
    "positive integers"
  )
})


test_that("frontier_neighbors constructs reproducible objective-space pairs", {
  x <- make_mock_solutionset()

  expect_named(
    formals(frontier_neighbors),
    c("x", "objectives", "method", "metric", "k")
  )

  for (method in c("knn", "sequence", "mst")) {
    neighbors <- frontier_neighbors(x, method = method)

    expect_s3_class(neighbors, "data.frame")
    expect_equal(nrow(neighbors), 1L)
    expect_false(anyDuplicated(names(neighbors)) > 0L)
    expect_equal(neighbors$from_solution, 2L)
    expect_equal(neighbors$to_solution, 1L)
    expect_equal(neighbors$objective_distance, sqrt(2))
    expect_equal(neighbors$improvement_cost, 2)
    expect_equal(neighbors$improvement_benefit, -4)
    expect_equal(attr(neighbors, "method"), method)
    expect_equal(attr(neighbors, "method_requested"), method)
    expect_equal(attr(neighbors, "order_objective"), "cost")
  }

  expect_error(
    frontier_neighbors(x, method = "knn", k = 0),
    "positive integer"
  )
})


test_that("frontier_neighbors uses the first objective for progression", {
  x <- make_mock_solutionset()

  by_cost <- frontier_neighbors(
    x,
    objectives = c("cost", "benefit"),
    method = "sequence"
  )

  by_benefit <- frontier_neighbors(
    x,
    objectives = c("benefit", "cost"),
    method = "sequence"
  )

  expect_equal(by_cost$from_solution, 2L)
  expect_equal(by_cost$to_solution, 1L)
  expect_equal(attr(by_cost, "order_objective"), "cost")

  expect_equal(by_benefit$from_solution, 1L)
  expect_equal(by_benefit$to_solution, 2L)
  expect_equal(attr(by_benefit, "order_objective"), "benefit")

  expect_equal(
    by_cost$objective_distance,
    by_benefit$objective_distance
  )
})


test_that("frontier_neighbors resolves the automatic method by dimension", {
  x <- make_mock_solutionset()

  two_objectives <- frontier_neighbors(x)
  expect_equal(attr(two_objectives, "method_requested"), "auto")
  expect_equal(attr(two_objectives, "method"), "sequence")
  expect_silent(frontier_neighbors(x, k = 0))

  third <- x$problem$data$objectives[["cost"]]
  third$alias <- "third"
  third$sense <- "min"
  x$problem$data$objectives[["third"]] <- third
  x$solution$runs$value_third <- c(3, 1)

  three_objectives <- frontier_neighbors(x)
  expect_equal(attr(three_objectives, "method_requested"), "auto")
  expect_equal(attr(three_objectives, "method"), "mst")
  expect_equal(
    attr(three_objectives, "objectives"),
    c("cost", "benefit", "third")
  )
})


test_that("frontier_neighbors retains stored dominated solutions and ties", {
  x <- make_mock_solutionset()
  x$solution$runs$value_cost <- c(1, 2)
  x$solution$runs$value_benefit <- c(3, 2)

  dominated <- frontier_neighbors(x, method = "sequence")
  expect_equal(nrow(dominated), 1L)
  expect_equal(
    sort(c(dominated$from_solution, dominated$to_solution)),
    c(1L, 2L)
  )

  x$solution$runs$value_cost <- c(1, 1)
  x$solution$runs$value_benefit <- c(3, 3)

  tied <- frontier_neighbors(x, method = "sequence")
  expect_equal(nrow(tied), 1L)
  expect_equal(tied$from_solution, 1L)
  expect_equal(tied$to_solution, 2L)
  expect_equal(tied$objective_distance, 0)
})


test_that("selection_consistency summarizes recurrence and disagreement", {
  x <- make_mock_solutionset()

  expect_named(
    formals(selection_consistency),
    c("x", "solution_groups")
  )

  consistency <- selection_consistency(x)

  expect_named(
    consistency,
    c(
      "pu", "solution_group", "n_solutions",
      "selected_frequency", "managed_frequency", "unmanaged_frequency",
      "dominant_state", "dominant_frequency", "dominant_tie",
      "n_states", "variable", "entropy", "normalized_entropy"
    )
  )
  expect_equal(nrow(consistency), 4L)
  expect_true(all(consistency$solution_group == "all"))
  expect_equal(consistency$n_solutions, rep(2L, 4L))
  expect_equal(consistency$selected_frequency, rep(0.5, 4L))
  expect_equal(consistency$managed_frequency, rep(0.5, 4L))
  expect_equal(consistency$unmanaged_frequency, rep(0.5, 4L))
  expect_equal(consistency$dominant_frequency, rep(0.5, 4L))
  expect_true(all(consistency$dominant_tie))
  expect_equal(
    consistency$dominant_state,
    c("conservation", "restoration", "conservation", "restoration")
  )
  expect_equal(consistency$n_states, rep(2L, 4L))
  expect_true(all(consistency$variable))
  expect_equal(consistency$entropy, rep(log(2), 4L))
  expect_equal(consistency$normalized_entropy, rep(1, 4L))
  expect_null(attr(consistency, "decision_scope"))
  expect_null(attr(consistency, "weighted"))
})


test_that("selection_consistency accepts named lists of numeric solution ids", {
  x <- make_mock_solutionset()

  grouped <- selection_consistency(
    x,
    solution_groups = list(cost = 1L, benefit = 2L)
  )

  expect_equal(nrow(grouped), 8L)
  expect_equal(unique(grouped$solution_group), c("cost", "benefit"))
  expect_true(all(grouped$n_solutions == 1L))
  expect_equal(grouped$selected_frequency, grouped$managed_frequency)
  expect_true(all(grouped$dominant_frequency == 1))
  expect_false(any(grouped$dominant_tie))
  expect_true(all(grouped$n_states == 1L))
  expect_false(any(grouped$variable))
  expect_true(all(grouped$entropy == 0))
  expect_true(all(grouped$normalized_entropy == 0))
  expect_equal(
    grouped$managed_frequency + grouped$unmanaged_frequency,
    rep(1, nrow(grouped))
  )
})


test_that("selection_consistency removes weights and decision scope", {
  x <- make_mock_solutionset()

  expect_error(
    selection_consistency(x, solution_weights = c(1, 1)),
    "unused argument"
  )
  expect_error(
    selection_consistency(x, decision_scope = "variable"),
    "unused argument"
  )
})


test_that("selection_consistency validates a complete numeric partition", {
  x <- make_mock_solutionset()

  expect_error(
    selection_consistency(
      x,
      solution_groups = data.frame(solution_id = 1:2, solution_group = "a")
    ),
    "named list"
  )
  expect_error(
    selection_consistency(x, solution_groups = list(1, 2)),
    "unique, non-empty group names"
  )
  expect_error(
    selection_consistency(x, solution_groups = list(a = integer(0), b = 1:2)),
    "at least one solution id"
  )
  expect_error(
    selection_consistency(x, solution_groups = list(a = "1", b = "2")),
    "numeric solution ids"
  )
  expect_error(
    selection_consistency(x, solution_groups = list(a = 1.5, b = 2)),
    "positive integer solution ids"
  )
  expect_error(
    selection_consistency(x, solution_groups = list(a = 0, b = 1:2)),
    "positive integer solution ids"
  )
  expect_error(
    selection_consistency(x, solution_groups = list(a = 1, b = NA_real_)),
    "positive integer solution ids"
  )
  expect_error(
    selection_consistency(x, solution_groups = list(a = 1)),
    "does not classify solution id"
  )
  expect_error(
    selection_consistency(x, solution_groups = list(a = 1:2, b = 2)),
    "occur in exactly one"
  )
  expect_error(
    selection_consistency(x, solution_groups = list(a = 1, b = 99)),
    "Unknown solution id"
  )
})

test_that("linkage_distances combines objective and decision changes", {
  x <- make_mock_solutionset()

  expect_named(
    formals(linkage_distances),
    c("x", "objectives", "pairs", "objective_metric", "decision_metric")
  )

  linkage <- linkage_distances(x)

  expect_equal(nrow(linkage), 1L)
  expect_false(anyDuplicated(names(linkage)) > 0L)
  expect_type(linkage$from_solution, "integer")
  expect_type(linkage$to_solution, "integer")
  expect_equal(linkage$from_solution, 2L)
  expect_equal(linkage$to_solution, 1L)
  expect_equal(linkage$objective_distance, sqrt(2))
  expect_equal(linkage$decision_similarity, 0)
  expect_equal(linkage$decision_distance, 1)
  expect_equal(linkage$changed_assignments, 4L)
  expect_equal(linkage$changed_planning_units, 4L)
  expect_equal(linkage$additions, 2L)
  expect_equal(linkage$removals, 2L)
  expect_equal(linkage$delta_cost, -2)
  expect_equal(linkage$improvement_cost, 2)
  expect_equal(linkage$delta_benefit, -4)
  expect_equal(linkage$improvement_benefit, -4)
  expect_equal(attr(linkage, "objective_metric"), "euclidean")
  expect_equal(attr(linkage, "decision_metric"), "jaccard")
  expect_null(attr(linkage, "decision_scope"))

  hamming <- linkage_distances(x, decision_metric = "hamming")
  expect_equal(hamming$decision_distance, 1)

  neighbors <- frontier_neighbors(x)
  local <- linkage_distances(x, pairs = neighbors)
  expect_equal(local$from_solution, neighbors$from_solution)
  expect_equal(local$to_solution, neighbors$to_solution)
})


test_that("linkage_distances supports objective-space metrics and orientation", {
  x <- make_mock_solutionset()

  euclidean <- linkage_distances(x, objective_metric = "euclidean")
  manhattan <- linkage_distances(x, objective_metric = "manhattan")
  chebyshev <- linkage_distances(x, objective_metric = "chebyshev")

  expect_equal(euclidean$objective_distance, sqrt(2))
  expect_equal(manhattan$objective_distance, 2)
  expect_equal(chebyshev$objective_distance, 1)
  expect_equal(attr(manhattan, "objective_metric"), "manhattan")
  expect_equal(attr(chebyshev, "objective_metric"), "chebyshev")

  benefit_first <- linkage_distances(
    x,
    objectives = c("benefit", "cost")
  )
  expect_equal(benefit_first$from_solution, 1L)
  expect_equal(benefit_first$to_solution, 2L)
  expect_equal(benefit_first$improvement_benefit, 4)
})


test_that("linkage_distances uses the complete assignment space", {
  x <- make_mock_solutionset()

  common_pu <- x$summary$pu[c(1L, 5L), , drop = FALSE]
  common_pu$solution_id <- c(1L, 2L)
  common_pu$internal_id <- 5L
  common_pu$id <- 5L
  common_pu$selected <- 1L
  x$summary$pu <- rbind(x$summary$pu, common_pu)

  common_action <- x$summary$actions[c(1L, 5L), , drop = FALSE]
  common_action$solution_id <- c(1L, 2L)
  common_action$internal_pu <- 5L
  common_action$pu <- 5L
  common_action$internal_action <- 1L
  common_action$action <- "conservation"
  common_action$selected <- 1L
  x$summary$actions <- rbind(x$summary$actions, common_action)

  jaccard <- linkage_distances(x, decision_metric = "jaccard")
  hamming <- linkage_distances(x, decision_metric = "hamming")

  expect_equal(jaccard$decision_similarity, 1 / 5)
  expect_equal(jaccard$decision_distance, 4 / 5)
  expect_equal(hamming$decision_similarity, 1 / 5)
  expect_equal(hamming$decision_distance, 4 / 5)
  expect_equal(jaccard$changed_assignments, 4L)
  expect_equal(jaccard$changed_planning_units, 4L)
})


test_that("linkage_distances generates and orients every unordered pair", {
  x <- make_mock_solutionset()

  third_run <- x$solution$runs[1L, , drop = FALSE]
  third_run$run_id <- 3L
  third_run$solution_id <- 3L
  third_run$value_cost <- 3
  third_run$value_benefit <- 7
  x$solution$runs <- rbind(x$solution$runs, third_run)
  x$solution$solutions[["3"]] <- x$solution$solutions[["1"]]

  third_actions <- x$summary$actions[
    x$summary$actions$solution_id == 1L,
    ,
    drop = FALSE
  ]
  third_actions$solution_id <- 3L
  x$summary$actions <- rbind(x$summary$actions, third_actions)

  distances <- linkage_distances(x)

  expect_equal(nrow(distances), choose(3, 2))
  expect_type(distances$from_solution, "integer")
  expect_type(distances$to_solution, "integer")
  expect_setequal(
    paste(distances$from_solution, distances$to_solution, sep = "->"),
    c("2->1", "3->1", "2->3")
  )
})


test_that("linkage_distances preserves non-consecutive numeric ids", {
  x <- make_mock_solutionset()

  x$solution$runs$run_id[2L] <- 3L
  x$solution$runs$solution_id[2L] <- 3L
  x$solution$solutions[["3"]] <- x$solution$solutions[["2"]]
  x$solution$solutions[["2"]] <- NULL

  for (table in names(x$summary)) {
    if ("solution_id" %in% names(x$summary[[table]])) {
      x$summary[[table]]$solution_id[
        x$summary[[table]]$solution_id == 2L
      ] <- 3L
    }
  }

  distances <- linkage_distances(
    x,
    pairs = data.frame(
      from_solution = 1L,
      to_solution = 3L
    )
  )

  expect_equal(distances$from_solution, 1L)
  expect_equal(distances$to_solution, 3L)
  expect_equal(distances$objective_distance, sqrt(2))
})

test_that("linkage_distances preserves supplied pair direction", {
  x <- make_mock_solutionset()
  manual <- data.frame(
    from_solution = 1L,
    to_solution = 2L,
    label = "forward"
  )

  directed <- linkage_distances(x, pairs = manual)

  expect_equal(directed$from_solution, 1L)
  expect_equal(directed$to_solution, 2L)
  expect_equal(directed$delta_cost, 2)
  expect_equal(directed$delta_benefit, 4)

  reciprocal <- linkage_distances(
    x,
    pairs = data.frame(
      from_solution = c(1L, 2L),
      to_solution = c(2L, 1L)
    )
  )

  expect_equal(reciprocal$from_solution, c(1L, 2L))
  expect_equal(reciprocal$to_solution, c(2L, 1L))
  expect_equal(reciprocal$objective_distance, rep(sqrt(2), 2))
  expect_equal(reciprocal$delta_cost, c(2, -2))
  expect_equal(reciprocal$delta_benefit, c(4, -4))
})


test_that("linkage_distances validates objectives and numeric pairs", {
  x <- make_mock_solutionset()

  expect_error(
    linkage_distances(x, objective_metric = "cosine"),
    "arg"
  )
  expect_error(
    linkage_distances(x, decision_metric = "cosine"),
    "arg"
  )
  expect_error(
    linkage_distances(x, decision_scope = "variable"),
    "unused argument"
  )
  expect_error(
    linkage_distances(x, objectives = 1:2),
    "character vector"
  )
  expect_error(
    linkage_distances(x, objectives = "cost"),
    "at least two aliases"
  )
  expect_error(
    linkage_distances(x, objectives = c("cost", "cost")),
    "unique aliases"
  )
  expect_error(
    linkage_distances(x, objectives = c("cost", NA_character_)),
    "missing or empty aliases"
  )
  expect_error(
    linkage_distances(x, objectives = c("cost", "unknown")),
    "Unknown objective"
  )
  expect_error(
    linkage_distances(x, pairs = matrix(c(1, 2), nrow = 1)),
    "data frame"
  )
  expect_error(
    linkage_distances(
      x,
      pairs = data.frame(from_solution = integer(0), to_solution = integer(0))
    ),
    "at least one row"
  )
  expect_error(
    linkage_distances(x, pairs = data.frame(from_solution = 1L)),
    "must contain"
  )
  expect_error(
    linkage_distances(
      x,
      pairs = data.frame(from_solution = "1", to_solution = "2")
    ),
    "numeric solution ids"
  )
  expect_error(
    linkage_distances(
      x,
      pairs = data.frame(from_solution = 1.5, to_solution = 2)
    ),
    "positive integer solution ids"
  )
  expect_error(
    linkage_distances(
      x,
      pairs = data.frame(from_solution = 0, to_solution = 2)
    ),
    "positive integer solution ids"
  )
  expect_error(
    linkage_distances(
      x,
      pairs = data.frame(from_solution = NA_real_, to_solution = 2)
    ),
    "positive integer solution ids"
  )
  expect_error(
    linkage_distances(
      x,
      pairs = data.frame(from_solution = Inf, to_solution = 2)
    ),
    "positive integer solution ids"
  )
  expect_error(
    linkage_distances(
      x,
      pairs = data.frame(from_solution = 1L, to_solution = 1L)
    ),
    "paired with itself"
  )
  expect_error(
    linkage_distances(
      x,
      pairs = data.frame(from_solution = 1L, to_solution = 99L)
    ),
    "Unknown solution id"
  )
  expect_error(
    linkage_distances(
      x,
      pairs = data.frame(
        from_solution = c(1L, 1L),
        to_solution = c(2L, 2L)
      )
    ),
    "duplicate directed pairs"
  )
})

test_that("linkage_transition decomposes a complete directed transition", {
  x <- make_mock_solutionset()

  transition <- linkage_transition(x, from = 1, to = 2)

  expect_equal(
    names(formals(linkage_transition)),
    c("x", "from", "to", "objectives")
  )
  expect_s3_class(transition, "multiscape_linkage_transition")
  expect_named(
    transition,
    c("summary", "objectives", "transitions", "actions", "state_matrix")
  )
  expect_named(
    transition$summary,
    c(
      "from_solution", "to_solution", "n_planning_units",
      "changed_planning_units", "unchanged_planning_units",
      "activated_planning_units", "deactivated_planning_units",
      "action_switches", "composition_changes",
      "additions", "removals"
    )
  )
  expect_type(transition$summary$from_solution, "integer")
  expect_type(transition$summary$to_solution, "integer")
  expect_equal(transition$summary$from_solution, 1L)
  expect_equal(transition$summary$to_solution, 2L)
  expect_false("objective_distance" %in% names(transition$summary))
  expect_false("decision_distance" %in% names(transition$summary))
  expect_null(attr(transition, "objective_metric"))
  expect_null(attr(transition, "decision_metric"))

  expect_named(
    transition$objectives,
    c(
      "objective", "sense", "from_value", "to_value",
      "change", "improvement", "normalized_improvement"
    )
  )
  expect_equal(transition$objectives$objective, c("cost", "benefit"))
  expect_equal(transition$objectives$change, c(2, 4))
  expect_equal(transition$objectives$improvement, c(-2, 4))
  expect_equal(transition$objectives$normalized_improvement, c(-1, 1))

  expect_equal(nrow(transition$transitions), 4L)
  expect_true(all(transition$transitions$changed))
  expect_equal(
    transition$transitions$transition,
    c("deactivated", "activated", "activated", "deactivated")
  )
  expect_equal(
    transition$transitions$added_actions,
    c(NA, "restoration", "conservation", NA)
  )
  expect_equal(
    transition$transitions$removed_actions,
    c("conservation", NA, NA, "restoration")
  )
  expect_true(all(is.na(transition$transitions$retained_actions)))

  expect_equal(
    sort(transition$actions$transition),
    sort(c("removed", "added", "added", "removed"))
  )
  expect_equal(sum(transition$state_matrix), 4L)
  expect_equal(transition$summary$n_planning_units, 4L)
  expect_equal(transition$summary$changed_planning_units, 4L)
  expect_equal(transition$summary$unchanged_planning_units, 0L)
  expect_equal(transition$summary$activated_planning_units, 2L)
  expect_equal(transition$summary$deactivated_planning_units, 2L)

  printed <- capture.output(print(transition))
  expect_true(any(grepl("Spatial solution transition", printed, fixed = TRUE)))
  expect_true(any(grepl("From solution: 1", printed, fixed = TRUE)))
  expect_true(any(grepl("Planning units changed: 4 of 4", printed, fixed = TRUE)))
  expect_true(any(grepl("Objective changes:", printed, fixed = TRUE)))
  expect_false(any(grepl("distance", printed, ignore.case = TRUE)))
})


test_that("linkage_transition validates directed numeric solution ids", {
  x <- make_mock_solutionset()

  for (bad in list(1:2, NA_real_, list(1), "1", 1.5, 0, -1, Inf)) {
    expect_error(
      linkage_transition(x, from = bad, to = 2),
      "single numeric positive-integer"
    )
  }

  for (bad in list(1:2, NA_real_, list(2), "2", 2.5, 0, -1, Inf)) {
    expect_error(
      linkage_transition(x, from = 1, to = bad),
      "single numeric positive-integer"
    )
  }

  expect_error(linkage_transition(x, from = 1, to = 1), "paired with itself")
  expect_error(linkage_transition(x, from = 1, to = 99), "Unknown solution id")
  expect_error(
    linkage_transition(x, from = 1, to = 2, objective_metric = "manhattan"),
    "unused argument"
  )
  expect_error(
    linkage_transition(x, from = 1, to = 2, decision_metric = "hamming"),
    "unused argument"
  )
  expect_error(
    linkage_transition(x, from = 1, to = 2, decision_scope = "variable"),
    "unused argument"
  )
  expect_error(
    linkage_transition(x, from = 1, to = 2, include_unchanged = FALSE),
    "unused argument"
  )
})


test_that("linkage_transition accepts one or more objective aliases", {
  x <- make_mock_solutionset()

  cost_only <- linkage_transition(
    x,
    from = 1,
    to = 2,
    objectives = "cost"
  )

  expect_equal(nrow(cost_only$objectives), 1L)
  expect_equal(cost_only$objectives$objective, "cost")
  expect_equal(cost_only$objectives$change, 2)

  benefit_first <- linkage_transition(
    x,
    from = 1,
    to = 2,
    objectives = c("benefit", "cost")
  )
  expect_equal(
    benefit_first$objectives$objective,
    c("benefit", "cost")
  )

  expect_error(
    linkage_transition(x, 1, 2, objectives = character()),
    "one or more aliases"
  )
  expect_error(
    linkage_transition(x, 1, 2, objectives = 1),
    "character vector"
  )
  expect_error(
    linkage_transition(x, 1, 2, objectives = c("cost", "cost")),
    "unique aliases"
  )
  expect_error(
    linkage_transition(x, 1, 2, objectives = ""),
    "missing or empty aliases"
  )
  expect_error(
    linkage_transition(x, 1, 2, objectives = "unknown"),
    "Unknown objective"
  )
})


test_that("linkage_transition preserves direction and non-consecutive ids", {
  x <- make_mock_solutionset()
  reverse <- linkage_transition(x, from = 2, to = 1)

  expect_equal(reverse$summary$from_solution, 2L)
  expect_equal(reverse$summary$to_solution, 1L)
  expect_equal(reverse$objectives$change, c(-2, -4))
  expect_equal(reverse$objectives$improvement, c(2, -4))

  x$solution$runs$run_id[2L] <- 3L
  x$solution$runs$solution_id[2L] <- 3L
  x$solution$solutions[["3"]] <- x$solution$solutions[["2"]]
  x$solution$solutions[["2"]] <- NULL

  for (table in names(x$summary)) {
    if ("solution_id" %in% names(x$summary[[table]])) {
      x$summary[[table]]$solution_id[
        x$summary[[table]]$solution_id == 2L
      ] <- 3L
    }
  }

  non_consecutive <- linkage_transition(x, from = 1, to = 3)
  expect_equal(non_consecutive$summary$from_solution, 1L)
  expect_equal(non_consecutive$summary$to_solution, 3L)
  expect_equal(nrow(non_consecutive$transitions), 4L)
})


test_that("linkage_turnover reports turnover and reconfiguration separately", {
  x <- make_mock_solutionset()

  expect_named(
    formals(linkage_turnover),
    c(
      "x", "objectives", "pairs", "objective_metric",
      "decision_metric", "tolerance"
    )
  )

  turnover <- linkage_turnover(x)

  expect_equal(nrow(turnover), 1L)
  expect_equal(turnover$decision_distance, 1)
  expect_false(turnover$objective_tie)
  expect_equal(turnover$reconfiguration_rate, 1 / sqrt(2))
  expect_false("turnover" %in% names(turnover))
  expect_false("turnover_ratio" %in% names(turnover))
  expect_equal(attr(turnover, "method"), "sequence")
  expect_equal(attr(turnover, "method_requested"), "auto")
  expect_equal(
    attr(turnover, "tolerance"),
    sqrt(.Machine$double.eps)
  )
  expect_equal(attr(turnover, "objective_metric"), "euclidean")
  expect_equal(attr(turnover, "decision_metric"), "jaccard")
})


test_that("linkage_turnover composes with explicit frontier pairs", {
  x <- make_mock_solutionset()
  third <- x$problem$data$objectives[["cost"]]
  third$alias <- "third"
  third$sense <- "min"
  x$problem$data$objectives[["third"]] <- third
  x$solution$runs$value_third <- c(3, 1)

  neighbors <- frontier_neighbors(
    x,
    objectives = c("cost", "benefit"),
    method = "knn"
  )
  turnover <- linkage_turnover(
    x,
    pairs = neighbors,
    objective_metric = "manhattan",
    decision_metric = "hamming"
  )

  expect_equal(attr(turnover, "objectives"), c("cost", "benefit"))
  expect_equal(attr(turnover, "method"), "knn")
  expect_equal(turnover$from_solution, neighbors$from_solution)
  expect_equal(turnover$to_solution, neighbors$to_solution)
  expect_equal(turnover$objective_distance, 2)
  expect_equal(turnover$decision_distance, 1)
  expect_equal(turnover$reconfiguration_rate, 0.5)
  expect_equal(attr(turnover, "objective_metric"), "manhattan")
  expect_equal(attr(turnover, "decision_metric"), "hamming")

  manual <- linkage_turnover(
    x,
    objectives = c("cost", "benefit"),
    pairs = data.frame(from_solution = 1, to_solution = 2)
  )
  expect_equal(manual$from_solution, 1L)
  expect_equal(manual$to_solution, 2L)
  expect_type(manual$from_solution, "integer")
  expect_type(manual$to_solution, "integer")
  expect_null(attr(manual, "method"))

  reverse <- linkage_turnover(
    x,
    objectives = c("cost", "benefit"),
    pairs = data.frame(
      from_solution = 2L,
      to_solution = 1L
    )
  )
  expect_equal(reverse$from_solution, 2L)
  expect_equal(reverse$to_solution, 1L)
  expect_equal(reverse$decision_distance, manual$decision_distance)
  expect_equal(
    reverse$reconfiguration_rate,
    manual$reconfiguration_rate
  )
})


test_that("linkage_turnover applies objective and decision metrics", {
  x <- make_mock_solutionset()

  extra_pu <- x$summary$pu[c(1L, 5L), , drop = FALSE]
  extra_pu$solution_id <- c(1L, 2L)
  extra_pu$internal_id <- 5L
  extra_pu$id <- 5L
  extra_pu$selected <- 0L
  x$summary$pu <- rbind(x$summary$pu, extra_pu)

  extra_action <- x$summary$actions[c(1L, 5L), , drop = FALSE]
  extra_action$solution_id <- c(1L, 2L)
  extra_action$internal_pu <- 5L
  extra_action$pu <- 5L
  extra_action$internal_action <- 1L
  extra_action$action <- "conservation"
  extra_action$selected <- 0L
  x$summary$actions <- rbind(x$summary$actions, extra_action)

  pair <- data.frame(from_solution = 1L, to_solution = 2L)
  euclidean <- linkage_turnover(x, pairs = pair)
  manhattan <- linkage_turnover(
    x, pairs = pair, objective_metric = "manhattan"
  )
  chebyshev <- linkage_turnover(
    x, pairs = pair, objective_metric = "chebyshev"
  )
  hamming <- linkage_turnover(
    x, pairs = pair, decision_metric = "hamming"
  )

  expect_equal(euclidean$objective_distance, sqrt(2))
  expect_equal(manhattan$objective_distance, 2)
  expect_equal(chebyshev$objective_distance, 1)
  expect_equal(euclidean$decision_distance, 1)
  expect_equal(hamming$decision_distance, 4 / 5)
  expect_equal(euclidean$reconfiguration_rate, 1 / sqrt(2))
  expect_equal(manhattan$reconfiguration_rate, 1 / 2)
  expect_equal(chebyshev$reconfiguration_rate, 1)
  expect_equal(hamming$reconfiguration_rate, (4 / 5) / sqrt(2))
})


test_that("linkage_turnover returns zero for unchanged decisions", {
  x <- make_mock_solutionset()
  x$summary$actions$selected[5:8] <-
    x$summary$actions$selected[1:4]

  unchanged <- linkage_turnover(x)

  expect_equal(unchanged$objective_distance, sqrt(2))
  expect_equal(unchanged$decision_distance, 0)
  expect_false(unchanged$objective_tie)
  expect_equal(unchanged$reconfiguration_rate, 0)
})

test_that("linkage_turnover identifies objective ties without infinite rates", {
  x <- make_mock_solutionset()
  x$solution$runs$value_cost <- c(1, 1)
  x$solution$runs$value_benefit <- c(3, 3)

  tied <- linkage_turnover(x, tolerance = 0)

  expect_equal(tied$objective_distance, 0)
  expect_equal(tied$decision_distance, 1)
  expect_true(tied$objective_tie)
  expect_true(is.na(tied$reconfiguration_rate))
  expect_false(any(is.infinite(tied$reconfiguration_rate)))

  x$summary$actions$selected[5:8] <- x$summary$actions$selected[1:4]
  identical <- linkage_turnover(x)
  expect_equal(identical$decision_distance, 0)
  expect_true(identical$objective_tie)
  expect_true(is.na(identical$reconfiguration_rate))
})


test_that("linkage_turnover applies tolerance in normalized objective space", {
  x <- make_mock_solutionset()

  third_run <- x$solution$runs[1L, , drop = FALSE]
  third_run$run_id <- 3L
  third_run$solution_id <- 3L
  third_run$value_cost <- 102
  third_run$value_benefit <- 205
  x$solution$runs <- rbind(x$solution$runs, third_run)
  x$solution$solutions[["3"]] <- x$solution$solutions[["1"]]

  for (table in names(x$summary)) {
    if ("solution_id" %in% names(x$summary[[table]])) {
      third_rows <- x$summary[[table]][
        x$summary[[table]]$solution_id == 1L,
        ,
        drop = FALSE
      ]
      third_rows$solution_id <- 3L
      x$summary[[table]] <- rbind(x$summary[[table]], third_rows)
    }
  }

  pair <- data.frame(from_solution = 1L, to_solution = 2L)
  below <- linkage_turnover(x, pairs = pair, tolerance = 0.02)
  above <- linkage_turnover(x, pairs = pair, tolerance = 0.03)

  expect_equal(
    below$objective_distance,
    sqrt(0.02^2 + 0.02^2)
  )
  expect_false(below$objective_tie)
  expect_true(is.finite(below$reconfiguration_rate))
  expect_true(above$objective_tie)
  expect_true(is.na(above$reconfiguration_rate))
})

test_that("linkage_turnover validates and applies tolerance", {
  x <- make_mock_solutionset()

  tied <- linkage_turnover(x, tolerance = sqrt(2))
  expect_true(tied$objective_tie)
  expect_true(is.na(tied$reconfiguration_rate))
  expect_equal(attr(tied, "tolerance"), sqrt(2))

  for (bad in list(NULL, -1, NA_real_, NaN, Inf, c(0, 1), TRUE, "small")) {
    expect_error(
      linkage_turnover(x, tolerance = bad),
      "single, finite, non-negative number"
    )
  }
  expect_error(
    linkage_turnover(x, objective_metric = "cosine"),
    "arg"
  )
  expect_error(
    linkage_turnover(x, decision_metric = "cosine"),
    "arg"
  )
  expect_error(
    linkage_turnover(x, objectives = "cost"),
    "at least two aliases"
  )
  expect_error(
    linkage_turnover(x, objectives = c("cost", "cost")),
    "unique aliases"
  )
  expect_error(
    linkage_turnover(x, decision_scope = "variable"),
    "unused argument"
  )
  expect_error(
    linkage_turnover(
      x,
      pairs = data.frame(
        from_solution = "1",
        to_solution = "2"
      )
    ),
    "numeric solution ids"
  )
})


test_that("linkage_contrasts ranks each supported contrast reproducibly", {
  linkage <- data.frame(
    from_solution = c(1, 2, 3, 4, 5),
    to_solution = c(2, 3, 4, 5, 6),
    objective_distance = c(0.5, 0.01, 0.2, 0, 0),
    decision_distance = c(0.9, 0.4, 0.2, 0.8, 0),
    reconfiguration_rate = c(1.8, 40, 1, NA, NA),
    objective_tie = c(FALSE, FALSE, FALSE, TRUE, TRUE)
  )

  objective_similar <- linkage_contrasts(
    linkage,
    type = "objective_similar",
    n = 5
  )
  expect_equal(objective_similar$from_solution, c(4L, 5L, 2L, 3L, 1L))

  decision_similar <- linkage_contrasts(
    linkage,
    type = "decision_similar",
    n = 5
  )
  expect_equal(decision_similar$from_solution, c(5L, 3L, 2L, 4L, 1L))

  high_turnover <- linkage_contrasts(
    linkage,
    type = "high_turnover",
    n = 5
  )
  expect_equal(high_turnover$from_solution, c(1L, 4L, 2L, 3L, 5L))

  high_reconfiguration <- linkage_contrasts(
    linkage,
    type = "high_reconfiguration",
    n = 5
  )
  expect_equal(high_reconfiguration$from_solution, c(2L, 1L, 3L))

  low_reconfiguration <- linkage_contrasts(
    linkage,
    type = "low_reconfiguration",
    n = 5
  )
  expect_equal(low_reconfiguration$from_solution, c(3L, 1L, 2L))

  objective_tie <- linkage_contrasts(
    linkage,
    type = "objective_tie",
    n = 5
  )
  expect_equal(objective_tie$from_solution, 4L)
  expect_type(objective_tie$from_solution, "integer")
  expect_type(objective_tie$to_solution, "integer")
  expect_equal(attr(objective_tie, "type"), "objective_tie")
  expect_null(attr(objective_tie, "tolerance"))
})

test_that("linkage_contrasts preserves input columns without hidden calculations", {
  distances <- linkage_distances(make_mock_solutionset())
  input_names <- names(distances)

  out <- linkage_contrasts(
    distances,
    type = "objective_similar",
    n = 1
  )

  expect_equal(
    names(out),
    c("contrast_rank", "contrast_type", input_names)
  )
  expect_false("objective_tie" %in% names(out))
  expect_false("reconfiguration_rate" %in% names(out))
  expect_type(out$from_solution, "integer")
  expect_type(out$to_solution, "integer")
})

test_that("linkage_contrasts requires turnover-derived columns when needed", {
  linkage <- data.frame(
    from_solution = 1:2,
    to_solution = 2:3,
    objective_distance = c(0, 0.2),
    decision_distance = c(0.8, 0.2)
  )

  expect_error(
    linkage_contrasts(linkage, type = "objective_tie"),
    "use `linkage_turnover\\(\\)` first"
  )
  expect_error(
    linkage_contrasts(linkage, type = "high_reconfiguration"),
    "use `linkage_turnover\\(\\)` first"
  )
  expect_error(
    linkage_contrasts(linkage, type = "low_reconfiguration"),
    "use `linkage_turnover\\(\\)` first"
  )

  linkage$objective_tie <- c(1, 0)
  expect_error(
    linkage_contrasts(linkage, type = "objective_tie"),
    "logical column"
  )

  linkage$objective_tie <- c(TRUE, FALSE)
  linkage$reconfiguration_rate <- c(NA, Inf)
  expect_error(
    linkage_contrasts(linkage, type = "high_reconfiguration"),
    "finite numbers or `NA`"
  )

  linkage$reconfiguration_rate <- c(NA, -1)
  expect_error(
    linkage_contrasts(linkage, type = "low_reconfiguration"),
    "finite numbers or `NA`"
  )
})

test_that("linkage_contrasts enforces numeric positive integer solution ids", {
  make_linkage <- function(from = 1, to = 2) {
    data.frame(
      from_solution = from,
      to_solution = to,
      objective_distance = 0.2,
      decision_distance = 0.4
    )
  }

  expect_error(
    linkage_contrasts(make_linkage("1", "2")),
    "numeric solution ids"
  )
  expect_error(
    linkage_contrasts(make_linkage(1.5, 2)),
    "positive integer solution ids"
  )
  expect_error(
    linkage_contrasts(make_linkage(0, 2)),
    "positive integer solution ids"
  )
  expect_error(
    linkage_contrasts(make_linkage(NA_real_, 2)),
    "positive integer solution ids"
  )
  expect_error(
    linkage_contrasts(make_linkage(Inf, 2)),
    "positive integer solution ids"
  )

  out <- linkage_contrasts(make_linkage(1, 2), n = 1)
  expect_identical(out$from_solution, 1L)
  expect_identical(out$to_solution, 2L)
})

test_that("linkage_contrasts validates distances, n, and removed arguments", {
  linkage <- data.frame(
    from_solution = 1L,
    to_solution = 2L,
    objective_distance = 0.2,
    decision_distance = 0.4
  )

  expect_error(
    linkage_contrasts(transform(linkage, objective_distance = -1)),
    "finite, non-negative"
  )
  expect_error(
    linkage_contrasts(transform(linkage, decision_distance = 1.1)),
    "between zero and one"
  )
  expect_error(linkage_contrasts(linkage, n = 0), "positive integer")
  expect_error(linkage_contrasts(linkage, n = 1.5), "positive integer")
  expect_error(linkage_contrasts(linkage, n = Inf), "positive integer")
  expect_error(
    linkage_contrasts(linkage, type = "low_turnover"),
    "arg"
  )
  expect_error(
    linkage_contrasts(linkage, tolerance = 1e-8),
    "unused argument"
  )
})

test_that("linkage_contrasts returns typed empty objective-tie results", {
  linkage <- data.frame(
    from_solution = c(1L, 2L),
    to_solution = c(2L, 3L),
    objective_distance = c(0, 0.2),
    decision_distance = c(0, 0.4),
    objective_tie = c(TRUE, FALSE)
  )

  out <- linkage_contrasts(
    linkage,
    type = "objective_tie",
    n = 3
  )

  expect_equal(nrow(out), 0L)
  expect_type(out$from_solution, "integer")
  expect_type(out$to_solution, "integer")
  expect_type(out$contrast_rank, "integer")
  expect_type(out$contrast_type, "character")
})

test_that("linkage_transition retains unchanged PUs and detects switches", {
  x <- make_mock_solutionset()
  actions <- expand.grid(
    solution_id = 1:2,
    pu = 1:2,
    action = c("conservation", "restoration"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  actions$selected <- 0L
  actions$selected[
    actions$solution_id == 1L & actions$pu == 1L &
      actions$action == "conservation"
  ] <- 1L
  actions$selected[
    actions$solution_id == 2L & actions$pu == 1L &
      actions$action == "restoration"
  ] <- 1L
  x$summary$actions <- actions

  transition <- linkage_transition(x, 1, 2)
  pu1 <- transition$transitions[transition$transitions$pu == 1, ]
  pu2 <- transition$transitions[transition$transitions$pu == 2, ]

  expect_equal(nrow(transition$transitions), 4L)
  expect_setequal(transition$transitions$pu, 1:4)
  expect_equal(pu1$transition, "switched")
  expect_equal(pu1$added_actions, "restoration")
  expect_equal(pu1$removed_actions, "conservation")
  expect_equal(pu2$transition, "unchanged")
  expect_false(pu2$changed)
  expect_equal(transition$summary$n_planning_units, 4L)
  expect_equal(transition$summary$changed_planning_units, 1L)
  expect_equal(transition$summary$unchanged_planning_units, 3L)
  expect_equal(transition$summary$action_switches, 1L)
  expect_equal(sum(transition$state_matrix), 4L)
})


test_that("linkage_transition explains multi-action composition changes", {
  x <- make_mock_solutionset()
  actions <- expand.grid(
    solution_id = 1:2,
    pu = 1:2,
    action = c("conservation", "restoration"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  actions$selected <- 0L

  actions$selected[
    actions$solution_id == 1L &
      actions$action == "conservation"
  ] <- 1L
  actions$selected[
    actions$solution_id == 2L & actions$pu == 1L
  ] <- 1L
  actions$selected[
    actions$solution_id == 1L & actions$pu == 2L
  ] <- 1L
  actions$selected[
    actions$solution_id == 2L & actions$pu == 2L &
      actions$action == "conservation"
  ] <- 1L
  x$summary$actions <- actions

  transition <- linkage_transition(x, 1, 2)
  pu1 <- transition$transitions[transition$transitions$pu == 1, ]
  pu2 <- transition$transitions[transition$transitions$pu == 2, ]

  expect_equal(
    transition$transitions$transition,
    c("composition_changed", "composition_changed", "unchanged", "unchanged")
  )
  expect_equal(pu1$added_actions, "restoration")
  expect_true(is.na(pu1$removed_actions))
  expect_equal(pu1$retained_actions, "conservation")
  expect_true(is.na(pu2$added_actions))
  expect_equal(pu2$removed_actions, "restoration")
  expect_equal(pu2$retained_actions, "conservation")
  expect_equal(transition$summary$composition_changes, 2L)
  expect_equal(
    sort(transition$actions$transition),
    sort(c("added", "removed", "retained", "retained"))
  )
})
