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



#' @title Get feature summary from a Solution
#'
#' @description
#' Extract the per-feature summary table from a [solution-class] object returned by [solve()].
#'
#' The returned table summarizes, for each feature, the total amount available in the
#' landscape together with the positive and negative contributions induced by the
#' selected actions in the solution.
#'
#' @param x A [solution-class] object returned by [solve()].
#' @param run Integer. For multi-run objects, the run index to extract. Default `NULL`
#'   (all runs if present).
#'
#' @return A `data.frame` with one row per feature. The returned columns typically include:
#' \itemize{
#'   \item `feature`: feature id,
#'   \item `feature_name`: feature name,
#'   \item `total_available`: total amount available in the landscape for that feature,
#'   \item `benefit`: positive contribution induced by selected actions,
#'   \item `loss`: negative contribution induced by selected actions,
#'   \item `net`: net contribution, computed as `benefit - loss`,
#'   \item `total`: total resulting amount, typically `total_available + net`.
#' }
#'
#' If the solution summary includes a `run_id` column and multiple runs are returned,
#' that column is preserved.
#'
#' @details
#' This function expects the feature summary table at `x$summary$features` and errors if
#' it is missing.
#'
#' The summary is intended to provide a user-facing overview of feature outcomes in the
#' solution. In particular:
#' \itemize{
#'   \item `total_available` refers to the total baseline amount available across the full landscape,
#'   \item `benefit` and `loss` summarize the action-induced positive and negative effects,
#'   \item `net` summarizes the overall balance of action effects,
#'   \item `total` combines the baseline amount and the net effect.
#' }
#'
#' This function is distinct from [get_targets()], which reports target achievement rather
#' than the overall effect balance by feature.
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

  out <- f

  # ensure core numeric columns exist
  if (!("total_available" %in% names(out))) out$total_available <- 0
  if (!("benefit" %in% names(out))) out$benefit <- 0
  if (!("loss" %in% names(out))) out$loss <- 0

  out$total_available <- as.numeric(out$total_available)
  out$benefit <- as.numeric(out$benefit)
  out$loss <- as.numeric(out$loss)

  if (!("net" %in% names(out))) {
    out$net <- out$benefit - out$loss
  } else {
    out$net <- as.numeric(out$net)
  }

  if (!("total" %in% names(out))) {
    out$total <- out$total_available + out$net
  } else {
    out$total <- as.numeric(out$total)
  }

  keep_first <- c(
    "run_id",
    "feature",
    "feature_name",
    "total_available",
    "benefit",
    "loss",
    "net",
    "total"
  )

  keep_first <- intersect(keep_first, names(out))
  keep_rest <- setdiff(names(out), keep_first)
  out <- out[, c(keep_first, keep_rest), drop = FALSE]

  if ("run_id" %in% names(out) && length(unique(out$run_id)) <= 1L && is.null(run)) {
    out$run_id <- NULL
  }

  out
}

#' @title Get target achievement table from a Solution
#'
#' @description
#' Extract the target achievement summary table from a [solution-class] object returned by [solve()].
#'
#' This function returns a simplified user-facing table with the most relevant target
#' information: feature id, feature name, target level, total available amount, target value,
#' achieved value, gap, and whether the target was met.
#'
#' @param x A [solution-class] object returned by [solve()].
#' @param run Integer. For multi-run objects, the run index to extract. Default `NULL`
#'   (all runs if present).
#'
#' @return A `data.frame` with a simplified target summary, or `NULL` if the solution
#'   does not contain a targets summary table.
#'
#' @details
#' Targets are optional. If the solution does not include `x$summary$targets`,
#' this function returns `NULL` without error.
#'
#' The returned table typically includes:
#' \itemize{
#'   \item `feature`: feature id,
#'   \item `feature_name`: feature name (if available),
#'   \item `target_level`: relative or absolute target level as stored in the target definition,
#'   \item `total_available`: total amount available in the landscape for that feature,
#'   \item `target`: target value,
#'   \item `achieved`: achieved value in the solution,
#'   \item `gap`: `achieved - target`,
#'   \item `met`: logical indicator of whether the target was met.
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

  out <- t

  if (all(c("achieved", "target_value") %in% names(out))) {
    if ("sense" %in% names(out)) {
      out$met <- ifelse(
        out$sense %in% c("ge", ">=", "min"),
        out$achieved >= out$target_value,
        ifelse(
          out$sense %in% c("le", "<=", "max"),
          out$achieved <= out$target_value,
          NA
        )
      )
    } else {
      out$met <- out$achieved >= out$target_value
    }
  }

  keep <- c(
    "run_id",
    "feature",
    "feature_name",
    "target_raw",
    "basis_total",
    "target_value",
    "achieved",
    "gap",
    "met"
  )
  keep <- intersect(keep, names(out))
  out <- out[, keep, drop = FALSE]

  names(out)[names(out) == "target_raw"] <- "target_level"
  names(out)[names(out) == "basis_total"] <- "total_available"
  names(out)[names(out) == "target_value"] <- "target"

  if ("run_id" %in% names(out) && length(unique(out$run_id)) <= 1L && is.null(run)) {
    out$run_id <- NULL
  }

  out
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
