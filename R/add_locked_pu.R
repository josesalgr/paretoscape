#' @include internal.R
NULL

#' @title Add locked planning units to a problem
#'
#' @description
#' Define planning units that must be included in, or excluded from, the
#' optimization problem.
#'
#' This function updates the planning-unit table stored in \code{x$data$pu} by
#' creating or replacing the logical columns \code{locked_in} and
#' \code{locked_out}. These columns are later used by the model builder when
#' translating the \code{Problem} object into optimization constraints.
#'
#' Lock information may be supplied directly as logical vectors, as vectors of
#' planning-unit ids, or by referencing columns in the raw planning-unit data
#' originally passed to \code{\link{inputData}}.
#' In addition, a Marxan-style \code{pu_status} specification can be provided,
#' where status code \code{2} denotes locked-in planning units and status code
#' \code{3} denotes locked-out planning units.
#'
#' @details
#' Let \eqn{\mathcal{P}} denote the set of planning units and let
#' \eqn{w_i \in \{0,1\}} denote the binary variable indicating whether planning
#' unit \eqn{i \in \mathcal{P}} is selected by the model.
#'
#' This function defines two subsets:
#' \itemize{
#'   \item \eqn{\mathcal{P}^{in} \subseteq \mathcal{P}}, the planning units that
#'   must be included,
#'   \item \eqn{\mathcal{P}^{out} \subseteq \mathcal{P}}, the planning units
#'   that must be excluded.
#' }
#'
#' Conceptually, these sets correspond to the following conditions:
#' \itemize{
#'   \item if \eqn{i \in \mathcal{P}^{in}}, then \eqn{w_i = 1},
#'   \item if \eqn{i \in \mathcal{P}^{out}}, then \eqn{w_i = 0}.
#' }
#'
#' These constraints are not imposed immediately by this function; instead, they
#' are stored in \code{x$data$pu$locked_in} and \code{x$data$pu$locked_out} and
#' enforced later when building the optimization model.
#'
#' \strong{Philosophy}
#'
#' The role of \code{\link{inputData}} is to construct and normalize the basic
#' inputs of the planning problem. Locking planning units is treated as a
#' separate modelling step so that users can define or revise selection
#' restrictions after the \code{Problem} object has already been created.
#'
#' \strong{Supported input formats}
#'
#' For \code{locked_in} and \code{locked_out}, the function accepts:
#' \itemize{
#'   \item \code{NULL}, meaning that no explicit update is supplied for that
#'   side,
#'   \item a single character string, interpreted as a column name in
#'   \code{x$data$pu_data_raw},
#'   \item a logical vector of length \code{nrow(x$data$pu)},
#'   \item a vector of planning-unit ids.
#' }
#'
#' For \code{pu_status}, the function accepts:
#' \itemize{
#'   \item \code{NULL},
#'   \item a single character string, interpreted as a column name in
#'   \code{x$data$pu_data_raw},
#'   \item a vector of length \code{nrow(x$data$pu)} containing status codes.
#' }
#'
#' When \code{pu_status} is used, values are interpreted as follows:
#' \itemize{
#'   \item \code{2}: locked in,
#'   \item \code{3}: locked out,
#'   \item all other values: treated as free.
#' }
#'
#' \strong{Priority rules}
#'
#' By default, explicit \code{locked_in} and \code{locked_out} inputs take
#' precedence over \code{pu_status}. This means that \code{pu_status} is first
#' translated into candidate locked-in and locked-out sets, and then explicit
#' \code{locked_in} and \code{locked_out} assignments are applied on top of
#' those values.
#'
#' If \code{status_overrides = TRUE}, then \code{pu_status} overrides any
#' existing or explicitly supplied \code{locked_in} and \code{locked_out}
#' assignments.
#'
#' \strong{Overwrite behaviour}
#'
#' If \code{overwrite = TRUE}, the function replaces any existing
#' \code{locked_in} and \code{locked_out} columns in \code{x$data$pu}.
#'
#' If \code{overwrite = FALSE}, new locked values are merged with any existing
#' values using logical OR. This means that already locked planning units remain
#' locked unless the object is rebuilt or overwritten explicitly.
#'
#' \strong{Consistency checks}
#'
#' The function checks that no planning unit is simultaneously assigned to both
#' \code{locked_in} and \code{locked_out}. If such conflicts are found, an error
#' is raised.
#'
#' @param x A \code{Problem} object created with \code{\link{inputData}}.
#'
#' @param locked_in Optional locked-in specification. It may be \code{NULL}, a
#'   column name in \code{x$data$pu_data_raw}, a logical vector, or a vector of
#'   planning-unit ids.
#'
#' @param locked_out Optional locked-out specification. It may be \code{NULL}, a
#'   column name in \code{x$data$pu_data_raw}, a logical vector, or a vector of
#'   planning-unit ids.
#'
#' @param pu_status Optional Marxan-style status specification. It may be
#'   \code{NULL}, a column name in \code{x$data$pu_data_raw}, or a vector of
#'   length \code{nrow(x$data$pu)}. Values \code{2} indicate locked-in planning
#'   units and values \code{3} indicate locked-out planning units.
#'
#' @param overwrite Logical. If \code{TRUE}, replace any existing
#'   \code{locked_in} and \code{locked_out} columns in \code{x$data$pu}. If
#'   \code{FALSE}, merge new values with existing ones using logical OR.
#'
#' @param status_overrides Logical. If \code{TRUE}, \code{pu_status} overrides
#'   explicit or existing \code{locked_in} and \code{locked_out} assignments. If
#'   \code{FALSE}, explicit \code{locked_in} and \code{locked_out} inputs take
#'   precedence over \code{pu_status}.
#'
#' @return An updated \code{Problem} object in which \code{x$data$pu} contains
#'   logical columns \code{locked_in} and \code{locked_out}.
#'
#' @examples
#' pu <- data.frame(
#'   id = 1:5,
#'   cost = c(2, 3, 1, 4, 2),
#'   lock_col = c(TRUE, FALSE, FALSE, TRUE, FALSE),
#'   status_col = c(0, 2, 0, 3, 0)
#' )
#'
#' features <- data.frame(
#'   id = c("sp1", "sp2")
#' )
#'
#' dist_features <- data.frame(
#'   pu = c(1, 1, 2, 3, 4, 5),
#'   feature = c("sp1", "sp2", "sp1", "sp2", "sp1", "sp2"),
#'   amount = c(1, 2, 1, 3, 2, 1)
#' )
#'
#' p <- inputData(
#'   pu = pu,
#'   features = features,
#'   dist_features = dist_features
#' )
#'
#' # 1) Lock by planning-unit ids
#' p1 <- add_locked_pu(
#'   x = p,
#'   locked_in = c(1, 3),
#'   locked_out = c(5)
#' )
#'
#' p1$data$pu[, c("id", "locked_in", "locked_out")]
#'
#' # 2) Read lock information from raw PU data columns
#' p2 <- add_locked_pu(
#'   x = p,
#'   locked_in = "lock_col",
#'   overwrite = TRUE
#' )
#'
#' p2$data$pu[, c("id", "locked_in", "locked_out")]
#'
#' # 3) Use Marxan-style status codes
#' p3 <- add_locked_pu(
#'   x = p,
#'   pu_status = "status_col",
#'   overwrite = TRUE
#' )
#'
#' p3$data$pu[, c("id", "locked_in", "locked_out")]
#'
#' # 4) Use logical vectors
#' p4 <- add_locked_pu(
#'   x = p,
#'   locked_in = c(TRUE, FALSE, TRUE, FALSE, FALSE),
#'   locked_out = c(FALSE, FALSE, FALSE, TRUE, FALSE),
#'   overwrite = TRUE
#' )
#'
#' p4$data$pu[, c("id", "locked_in", "locked_out")]
#'
#' @seealso
#' \code{\link{inputData}},
#' \code{\link{add_actions}},
#' \code{\link{add_locked_actions}}
#'
#' @export
add_locked_pu <- function(
    x,
    locked_in = NULL,
    locked_out = NULL,
    pu_status = NULL,
    overwrite = TRUE,
    status_overrides = FALSE
) {
  stopifnot(inherits(x, "Problem"))

  x <- .pa_clone_data(x)

  pu <- x$data$pu %||% NULL
  if (is.null(pu) || !inherits(pu, "data.frame") || nrow(pu) == 0L) {
    stop("x$data$pu is missing or empty.", call. = FALSE)
  }
  if (!("id" %in% names(pu))) {
    stop("x$data$pu must contain column 'id'.", call. = FALSE)
  }

  pu_raw <- x$data$pu_data_raw %||% NULL
  n_pu <- nrow(pu)
  pu_ids <- as.integer(pu$id)

  .as_logical_from_column <- function(colname, source_df, what) {
    if (is.null(source_df) || !inherits(source_df, "data.frame")) {
      stop(
        what, " was provided as a column name, but x$data$pu_data_raw is missing.",
        call. = FALSE
      )
    }

    colname <- as.character(colname)[1]
    if (is.na(colname) || !nzchar(colname)) {
      stop(what, " column name must be a non-empty string.", call. = FALSE)
    }
    if (!(colname %in% names(source_df))) {
      stop(
        what, " column '", colname, "' was not found in x$data$pu_data_raw.",
        call. = FALSE
      )
    }

    v <- source_df[[colname]]

    if (is.logical(v)) {
      out <- v
    } else if (is.numeric(v) || is.integer(v)) {
      out <- v != 0
    } else if (is.factor(v)) {
      v <- as.character(v)
      vv <- tolower(trimws(v))
      out <- vv %in% c("true", "t", "1", "yes", "y")
    } else if (is.character(v)) {
      vv <- tolower(trimws(v))
      out <- vv %in% c("true", "t", "1", "yes", "y")
    } else {
      stop(
        what, " column '", colname, "' could not be coerced to logical.",
        call. = FALSE
      )
    }

    out <- as.logical(out)
    out[is.na(out)] <- FALSE

    if (length(out) != n_pu) {
      stop(
        what, " column '", colname, "' has length ", length(out),
        " but expected ", n_pu, ".",
        call. = FALSE
      )
    }

    out
  }

  .as_status_vector <- function(z) {
    if (is.null(z)) return(NULL)

    if (is.character(z) && length(z) == 1L) {
      if (is.null(pu_raw) || !inherits(pu_raw, "data.frame")) {
        stop(
          "pu_status was provided as a column name, but x$data$pu_data_raw is missing.",
          call. = FALSE
        )
      }
      if (!(z %in% names(pu_raw))) {
        stop("pu_status column '", z, "' was not found in x$data$pu_data_raw.", call. = FALSE)
      }
      z <- pu_raw[[z]]
    }

    z <- as.integer(z)
    if (length(z) != n_pu) {
      stop(
        "pu_status must have length ", n_pu, ".",
        call. = FALSE
      )
    }

    z
  }

  .resolve_lock_spec <- function(spec, what) {
    if (is.null(spec)) return(NULL)

    # case 1: single character -> column name in pu_data_raw
    if (is.character(spec) && length(spec) == 1L && !spec %in% as.character(pu_ids)) {
      return(.as_logical_from_column(spec, pu_raw, what))
    }

    # case 2: logical vector
    if (is.logical(spec)) {
      if (length(spec) != n_pu) {
        stop(
          what, " logical vector must have length ", n_pu, ".",
          call. = FALSE
        )
      }
      spec <- as.logical(spec)
      spec[is.na(spec)] <- FALSE
      return(spec)
    }

    # case 3: PU ids
    ids <- suppressWarnings(as.integer(spec))
    if (length(ids) == 0L || all(is.na(ids))) {
      stop(
        what, " must be NULL, a column name, a logical vector, or a vector of PU ids.",
        call. = FALSE
      )
    }
    ids <- unique(ids[!is.na(ids)])

    bad <- ids[!ids %in% pu_ids]
    if (length(bad) > 0L) {
      stop(
        what, " contains PU ids not present in x$data$pu$id: ",
        paste(bad, collapse = ", "),
        call. = FALSE
      )
    }

    pu_ids %in% ids
  }

  lock_in_vec  <- .resolve_lock_spec(locked_in,  "locked_in")
  lock_out_vec <- .resolve_lock_spec(locked_out, "locked_out")
  status_vec   <- .as_status_vector(pu_status)

  status_in  <- if (!is.null(status_vec)) status_vec == 2L else NULL
  status_out <- if (!is.null(status_vec)) status_vec == 3L else NULL

  # current values if they already exist
  current_in <- if ("locked_in" %in% names(pu)) {
    as.logical(pu$locked_in)
  } else {
    rep(FALSE, n_pu)
  }
  current_out <- if ("locked_out" %in% names(pu)) {
    as.logical(pu$locked_out)
  } else {
    rep(FALSE, n_pu)
  }

  current_in[is.na(current_in)] <- FALSE
  current_out[is.na(current_out)] <- FALSE

  if (isTRUE(overwrite)) {
    new_in  <- rep(FALSE, n_pu)
    new_out <- rep(FALSE, n_pu)
  } else {
    new_in  <- current_in
    new_out <- current_out
  }

  if (!is.null(status_in) && isTRUE(status_overrides)) {
    new_in  <- status_in
    new_out <- status_out
  } else {
    if (!is.null(status_in)) {
      new_in  <- new_in  | status_in
      new_out <- new_out | status_out
    }
    if (!is.null(lock_in_vec)) {
      if (isTRUE(overwrite)) {
        new_in <- lock_in_vec
      } else {
        new_in <- new_in | lock_in_vec
      }
    }
    if (!is.null(lock_out_vec)) {
      if (isTRUE(overwrite)) {
        new_out <- lock_out_vec
      } else {
        new_out <- new_out | lock_out_vec
      }
    }
  }

  if (any(new_in & new_out, na.rm = TRUE)) {
    bad_ids <- pu_ids[new_in & new_out]
    stop(
      "Some planning units are both locked_in and locked_out: ",
      paste(utils::head(bad_ids, 20), collapse = ", "),
      if (length(bad_ids) > 20) " ..." else "",
      call. = FALSE
    )
  }

  x$data$pu$locked_in <- as.logical(new_in)
  x$data$pu$locked_out <- as.logical(new_out)

  if (!is.null(x$data$model_ptr)) {
    x$data$meta <- x$data$meta %||% list()
    x$data$meta$model_dirty <- TRUE
  }

  x
}
