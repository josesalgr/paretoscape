#' @include internalMO.R
#'
#' @title Get planning unit results from a Solution
#'
#' @description
#' Extract the planning-unit summary table from a [solution-class] object returned by [solve()].
#' The returned table includes a \code{selected} indicator (typically \code{0/1}) showing
#' whether each planning unit is selected in the solution.
#'
#' @param x A [solution-class] object returned by [solve()].
#' @param only_selected Logical. If \code{TRUE}, return only rows where \code{selected == 1}.
#'   Default \code{FALSE}.
#' @param run Integer. For multi-run objects, the run index to extract. Default \code{1L}.
#'
#' @return A \code{data.frame} with planning-unit information stored in the solution summary and
#'   a \code{selected} column.
#'
#' @details
#' This function expects the solution object to store a planning-unit summary table at
#' \code{x$summary$pu}. It errors if the table is missing. If \code{only_selected = TRUE},
#' it also errors when the \code{selected} column is not present.
#'
#' @examples
#' \dontrun{
#' sol <- solve(problem)
#' pu_tbl <- get_pu(sol)
#' pu_sel <- get_pu(sol, only_selected = TRUE)
#' }
#'
#' @export
#'
#' @seealso [get_actions()], [get_features()], [get_targets()], [get_solution_vector()]
get_pu <- function(x, only_selected = FALSE, run = NULL) {

  if (!inherits(x, c("Solution", "SolutionSet"))) {
    stop("x must be a Solution or SolutionSet.", call. = FALSE)
  }

  pu <- x$summary$pu %||% NULL
  if (is.null(pu)) {
    stop("No PU summary found (x$summary$pu is NULL).", call. = FALSE)
  }

  if (inherits(x, "SolutionSet") && !is.null(run)) {
    run <- as.integer(run)[1]
    if (!is.finite(run) || is.na(run) || run < 1L) {
      stop("run must be a positive integer (1-based).", call. = FALSE)
    }
    if (!("run_id" %in% names(pu))) {
      stop("PU summary has no 'run_id' column.", call. = FALSE)
    }
    pu <- pu[pu$run_id == run, , drop = FALSE]
  }

  if (isTRUE(only_selected)) {
    if (!("selected" %in% names(pu))) {
      stop("PU summary has no 'selected' column.", call. = FALSE)
    }
    pu <- pu[pu$selected == 1L, , drop = FALSE]
  }

  pu
}

#' @title Get action results from a Solution
#'
#' @description
#' Extract the action-allocation summary table from a [solution-class] object returned by [solve()].
#' The returned table includes a \code{selected} indicator (typically \code{0/1}) showing whether
#' each feasible \code{(pu, action)} pair is selected in the solution.
#'
#' @param x A [solution-class] object returned by [solve()].
#' @param only_selected Logical. If \code{TRUE}, return only rows where \code{selected == 1}.
#'   Default \code{FALSE}.
#' @param run Integer. For multi-run objects, the run index to extract. Default \code{1L}.
#'
#' @return A \code{data.frame} with action-allocation information stored in the solution summary and
#'   a \code{selected} column.
#'
#' @details
#' This function expects the solution object to store an action-allocation summary table at
#' \code{x$summary$actions}. It errors if the table is missing. If \code{only_selected = TRUE},
#' it also errors when the \code{selected} column is not present.
#'
#' @examples
#' \dontrun{
#' sol <- solve(problem)
#' act_tbl <- get_actions(sol)
#' act_sel <- get_actions(sol, only_selected = TRUE)
#' }
#'
#' @export
#'
#' @seealso [get_pu()], [get_features()], [get_targets()], [get_solution_vector()]
get_actions <- function(x, only_selected = FALSE, run = NULL) {

  if (!inherits(x, c("Solution", "SolutionSet"))) {
    stop("x must be a Solution or SolutionSet.", call. = FALSE)
  }

  a <- x$summary$actions %||% NULL
  if (is.null(a)) {
    stop("No actions summary found (x$summary$actions is NULL).", call. = FALSE)
  }

  if (inherits(x, "SolutionSet") && !is.null(run)) {
    run <- as.integer(run)[1]
    if (!is.finite(run) || is.na(run) || run < 1L) {
      stop("run must be a positive integer (1-based).", call. = FALSE)
    }
    if (!("run_id" %in% names(a))) {
      stop("Actions summary has no 'run_id' column.", call. = FALSE)
    }
    a <- a[a$run_id == run, , drop = FALSE]
  }

  if (isTRUE(only_selected)) {
    if (!("selected" %in% names(a))) {
      stop("Actions summary has no 'selected' column.", call. = FALSE)
    }
    a <- a[a$selected == 1L, , drop = FALSE]
  }

  a
}

#' @title Get feature achievement summary from a Solution
#'
#' @description
#' Extract the feature achievement summary table from a [solution-class] object returned by [solve()].
#' This table typically summarizes, for each feature, how much is achieved by baseline selection
#' and by action effects, and their total.
#'
#' @param x A [solution-class] object returned by [solve()].
#' @param run Integer. For multi-run objects, the run index to extract. Default \code{1L}.
#'
#' @return A \code{data.frame} with feature achievement metrics stored in the solution summary.
#'
#' @details
#' This function expects the feature achievement summary table at \code{x$summary$features} and
#' errors if it is missing. The exact columns depend on the model and reporting options, but
#' commonly include:
#' \itemize{
#' \item \code{baseline_contrib}: contribution from baseline / conservation selection.
#' \item \code{recovery_contrib}: contribution from action effects (e.g., benefits).
#' \item \code{total}: baseline + recovery.
#' }
#'
#' @examples
#' \dontrun{
#' sol <- solve(problem)
#' feat_tbl <- get_features(sol)
#' }
#'
#' @export
#'
#' @seealso [get_pu()], [get_actions()], [get_targets()]
get_features <- function(x, run = NULL) {

  if (!inherits(x, c("Solution", "SolutionSet"))) {
    stop("x must be a Solution or SolutionSet.", call. = FALSE)
  }

  f <- x$summary$features %||% NULL
  if (is.null(f)) {
    stop("No features summary found (x$summary$features is NULL).", call. = FALSE)
  }

  if (inherits(x, "SolutionSet") && !is.null(run)) {
    run <- as.integer(run)[1]
    if (!is.finite(run) || is.na(run) || run < 1L) {
      stop("run must be a positive integer (1-based).", call. = FALSE)
    }
    if (!("run_id" %in% names(f))) {
      stop("Features summary has no 'run_id' column.", call. = FALSE)
    }
    f <- f[f$run_id == run, , drop = FALSE]
  }

  f
}

#' @title Get target achievement table from a Solution
#'
#' @description
#' Extract the target achievement summary table (if present) from a [solution-class] object returned by [solve()].
#' The targets table typically contains the target value, achieved value, and gap
#' (\code{achieved - target_value}), plus target metadata such as type and units.
#'
#' @param x A [solution-class] object returned by [solve()].
#' @param run Integer. For multi-run objects, the run index to extract. Default \code{1L}.
#'
#' @return A \code{data.frame} with target achievement metrics, or \code{NULL} if the solution
#'   does not contain a targets summary table.
#'
#' @details
#' Targets are optional. If the solution does not include \code{x$summary$targets},
#' this function returns \code{NULL} without error.
#'
#' @examples
#' \dontrun{
#' sol <- solve(problem)
#' tgt_tbl <- get_targets(sol)
#' if (!is.null(tgt_tbl)) head(tgt_tbl)
#' }
#'
#' @export
#'
#' @seealso [get_pu()], [get_actions()], [get_features()]
get_targets <- function(x, run = NULL) {

  if (!inherits(x, c("Solution", "SolutionSet"))) {
    stop("x must be a Solution or SolutionSet.", call. = FALSE)
  }

  t <- x$summary$targets %||% NULL
  if (is.null(t)) {
    return(NULL)
  }

  if (inherits(x, "SolutionSet") && !is.null(run)) {
    run <- as.integer(run)[1]
    if (!is.finite(run) || is.na(run) || run < 1L) {
      stop("run must be a positive integer (1-based).", call. = FALSE)
    }
    if (!("run_id" %in% names(t))) {
      stop("Targets summary has no 'run_id' column.", call. = FALSE)
    }
    t <- t[t$run_id == run, , drop = FALSE]
  }

  t
}

#' @title Get raw solution vector from a Solution
#'
#' @description
#' Return the raw decision-variable vector produced by the solver, in the internal model
#' variable order used by the optimizer backend.
#'
#' @param x A [solution-class] object returned by [solve()].
#' @param run Integer. For multi-run objects, the run index to extract. Default \code{1L}.
#'
#' @return A numeric vector with one value per model variable.
#'
#' @details
#' This function expects the raw solution vector at \code{x$solution$vector} and errors if it is missing.
#' The vector is returned as numeric and corresponds to the model's variable ordering (e.g.,
#' planning-unit selection variables, action variables, and any auxiliary variables such as
#' fragmentation variables when present).
#'
#' @examples
#' \dontrun{
#' sol <- solve(problem)
#' v <- get_solution_vector(sol)
#' length(v)
#' }
#'
#' @export
#'
#' @seealso [get_pu()], [get_actions()], [get_features()], [get_targets()]
get_solution_vector <- function(x, run = 1L) {
  sol <- .mo_get_solution_from(x, run = run)

  v <- sol$solution$vector %||% NULL
  if (is.null(v)) {
    stop("No raw solution vector found (x$solution$vector is NULL).", call. = FALSE)
  }

  as.numeric(v)
}
