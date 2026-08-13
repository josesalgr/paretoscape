#' @include internal.R
NULL

#' @title Print
#'
#' @description Displays information about an object.
#'
#' @param x Any object.
#' @param ... Not used.
#'
#' @name print
#'
#' @return None.
#'
#' @seealso [base::print()].
#'
#' @aliases print
#' @keywords internal

NULL

#' @rdname print
#' @method print Problem
#' @export
print.Problem <- function(x, ...) x$print()


#' @rdname print
#' @method print SolutionSet
#' @export
print.SolutionSet <- function(x, ...) x$print()


#' @rdname print
#' @method print multiscape_linkage_transition
#' @export
print.multiscape_linkage_transition <- function(x, ...) {
  summary <- x$summary[1L, , drop = FALSE]

  changed_pct <- if (
    is.finite(summary$n_planning_units) &&
    summary$n_planning_units > 0
  ) {
    100 * summary$changed_planning_units / summary$n_planning_units
  } else {
    NA_real_
  }

  cat("Spatial solution transition\n")
  cat("From solution:", summary$from_solution, "\n")
  cat("To solution:  ", summary$to_solution, "\n\n")


  cat(
    "Planning units changed:",
    summary$changed_planning_units,
    "of",
    summary$n_planning_units
  )

  if (is.finite(changed_pct)) {
    cat(
      " (",
      format(round(changed_pct, 1), trim = TRUE),
      "%)",
      sep = ""
    )
  }

  cat("\n")

  cat("Activated:           ", summary$activated_planning_units, "\n")
  cat("Deactivated:         ", summary$deactivated_planning_units, "\n")
  cat("Action switches:     ", summary$action_switches, "\n")
  cat("Composition changes: ", summary$composition_changes, "\n")

  if (
    inherits(x$objectives, "data.frame") &&
    nrow(x$objectives) > 0L
  ) {
    cat("\nObjective changes:\n")

    for (i in seq_len(nrow(x$objectives))) {
      cat(
        "- ",
        x$objectives$objective[i],
        ": ",
        format(
          x$objectives$change[i],
          digits = 6,
          trim = TRUE
        ),
        " (improvement ",
        format(
          x$objectives$improvement[i],
          digits = 6,
          trim = TRUE
        ),
        ")\n",
        sep = ""
      )
    }
  }

  cat(
    "\nUse `$summary`, `$objectives`, `$transitions`, `$actions`, ",
    "or `$state_matrix` for details.\n",
    sep = ""
  )

  invisible(x)
}
