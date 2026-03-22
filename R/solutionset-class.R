#' @include internal.R
#'
#' @export
if (!methods::isClass("SolutionSet")) methods::setOldClass("SolutionSet")
NULL

#' SolutionSet class
#'
#' @description
#' The `SolutionSet` class stores the output of solving a `Problem` object in
#' `mosap` when multiple runs or multiple objective trade-offs are produced.
#' It contains the original problem, the multi-run solution content, user-facing
#' summaries, diagnostics, optimization method metadata, and additional metadata.
#'
#' Objects of this class are typically created with [solve()].
#'
#' @section Fields:
#' \describe{
#'   \item{problem}{The `Problem` object used to generate the solution set.}
#'   \item{solution}{A named `list` containing the core multi-run optimization
#'   outputs, typically including the experimental design, run summary table,
#'   and the list of individual `Solution` objects.}
#'   \item{summary}{A named `list` containing user-facing summaries derived from
#'   the solution set, intended for reporting, plotting, and inspection.}
#'   \item{diagnostics}{A named `list` containing diagnostics describing the
#'   solution set as a whole, such as number of runs, status summaries, and
#'   runtime/gap ranges.}
#'   \item{method}{A named `list` describing the multi-objective optimization
#'   method used to obtain the solution set.}
#'   \item{meta}{A named `list` containing additional metadata associated with
#'   the solution set.}
#'   \item{name}{A `character(1)` name for the solution set object.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{print()}{Print a concise summary of the solution set, including method,
#'   content, and run-level diagnostics.}
#'   \item{show()}{Alias of `print()`.}
#'   \item{repr()}{Returns a short string representation.}
#'   \item{getMethod()}{Return the method specification.}
#'   \item{getDesign()}{Return the design table stored in `solution$design`.}
#'   \item{getRuns()}{Return the run summary table stored in `solution$runs`.}
#'   \item{getSolutions()}{Return the list of `Solution` objects stored in
#'   `solution$solutions`.}
#' }
#'
#' @return No return value.
#'
#' @name solutionset-class
#' @aliases SolutionSet
NULL

.pa_solutionset_method_label <- function(method) {
  if (is.null(method) || !is.list(method)) return("unknown")
  as.character(method$name %||% method$type %||% "unknown")[1]
}

.pa_solutionset_aliases <- function(method) {
  if (is.null(method) || !is.list(method)) return(character(0))
  aliases <- as.character(method$aliases %||% character(0))
  aliases[!is.na(aliases) & nzchar(aliases)]
}

.pa_solutionset_preview_runs <- function(runs, max_show = 6L) {
  if (is.null(runs) || !inherits(runs, "data.frame") || nrow(runs) == 0L) {
    return(NULL)
  }

  val_cols <- grep("^value_", names(runs), value = TRUE)

  if (length(val_cols) == 0L) {
    return(NULL)
  }

  rr <- runs[1, , drop = FALSE]

  txt <- paste0(
    sub("^value_", "", val_cols),
    ": ",
    vapply(rr[val_cols], .pa_num_text, character(1)),
    collapse = ", "
  )

  txt
}

.pa_solutionset_status_summary <- function(runs) {
  if (is.null(runs) || !inherits(runs, "data.frame") || nrow(runs) == 0L || !("status" %in% names(runs))) {
    return("none")
  }

  st <- as.character(runs$status)
  st <- st[!is.na(st) & nzchar(st)]

  if (length(st) == 0L) return("none")

  tb <- table(st)
  paste0(names(tb), ": ", as.integer(tb), collapse = ", ")
}

.pa_solutionset_range_text <- function(x, digits = 3) {
  if (is.null(x)) return("none")
  x <- as.numeric(x)
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) == 0L) return("none")
  if (length(x) == 1L) return(as.character(round(x, digits)))
  paste0(round(min(x), digits), "..", round(max(x), digits))
}

.pa_solutionset_run_columns <- function(runs) {
  if (is.null(runs) || !inherits(runs, "data.frame") || nrow(runs) == 0L) {
    return(list(design = character(0), values = character(0), core = character(0)))
  }

  nm <- names(runs)

  core_cols <- intersect(c("run_id", "status", "runtime", "gap"), nm)
  design_cols <- grep("^(weight_|eps_)", nm, value = TRUE)
  value_cols <- grep("^value_", nm, value = TRUE)

  list(
    core = core_cols,
    design = design_cols,
    values = value_cols
  )
}

.pa_solutionset_preview_table <- function(runs, max_rows = 6L, digits = 3) {
  if (is.null(runs) || !inherits(runs, "data.frame") || nrow(runs) == 0L) {
    return(NULL)
  }

  cols <- .pa_solutionset_run_columns(runs)
  keep <- c(cols$core, cols$design, cols$values)
  keep <- unique(keep[keep %in% names(runs)])

  if (length(keep) == 0L) return(NULL)

  out <- runs[seq_len(min(nrow(runs), max_rows)), keep, drop = FALSE]

  for (nm in names(out)) {
    if (is.numeric(out[[nm]])) {
      out[[nm]] <- round(out[[nm]], digits)
    }
  }

  out
}

.pa_solutionset_best_run <- function(runs, method = NULL) {
  if (is.null(runs) || !inherits(runs, "data.frame") || nrow(runs) == 0L) {
    return(NULL)
  }

  ok <- rep(TRUE, nrow(runs))
  if ("status" %in% names(runs)) {
    ok <- runs$status %in% c("optimal", "time_limit_feasible", "solution_limit")
    ok[is.na(ok)] <- FALSE
  }

  rr <- runs[ok, , drop = FALSE]
  if (nrow(rr) == 0L) return(NULL)

  mname <- .pa_solutionset_method_label(method)

  if (identical(mname, "weighted")) {
    val_cols <- grep("^value_", names(rr), value = TRUE)
    if (length(val_cols) > 0L) {
      idx <- 1L
      out <- rr[idx, , drop = FALSE]
      return(out)
    }
  }

  rr[1, , drop = FALSE]
}

#' @export
SolutionSet <- pproto(
  "SolutionSet",
  problem = NULL,
  solution = list(),
  summary = list(),
  diagnostics = list(),
  method = list(),
  meta = list(),
  name = "solset",

  print = function(self) {
    ch <- .pa_cli_box_chars()
    div_id <- cli::cli_div(theme = .pa_cli_theme())

    method_name <- .pa_solutionset_method_label(self$method)
    aliases <- .pa_solutionset_aliases(self$method)

    design <- self$solution$design %||% NULL
    runs <- self$solution$runs %||% NULL
    sols <- self$solution$solutions %||% list()

    n_design <- if (inherits(design, "data.frame")) nrow(design) else 0L
    n_runs <- if (inherits(runs, "data.frame")) nrow(runs) else 0L
    n_solutions <- length(sols)

    alias_txt <- if (length(aliases) == 0L) {
      "none"
    } else {
      .pa_preview_text(aliases, max_show = 6L, quote = FALSE)
    }

    status_txt <- .pa_solutionset_status_summary(runs)

    runtime_txt <- if (!is.null(runs) && "runtime" %in% names(runs)) {
      .pa_solutionset_range_text(runs$runtime, digits = 3)
    } else {
      "none"
    }

    gap_txt <- if (!is.null(runs) && "gap" %in% names(runs)) {
      .pa_solutionset_range_text(runs$gap, digits = 4)
    } else {
      "none"
    }

    run_cols <- .pa_solutionset_run_columns(runs)
    design_txt <- if (length(run_cols$design) == 0L) "none" else paste(run_cols$design, collapse = ", ")
    value_txt  <- if (length(run_cols$values) == 0L) "none" else paste(run_cols$values, collapse = ", ")

    cli::cli_text("A mosap solution set ({.cls SolutionSet})")

    # ---- METHOD
    cli::cli_text("{ch$j}{ch$b}{.h method}", .envir = environment())
    cli::cli_text(
      "{ch$v}{ch$j}{ch$b}name:            {.code {method_name}}",
      .envir = environment()
    )
    cli::cli_text(
      "{ch$v}{ch$l}{ch$b}objectives:      {length(aliases)} ({alias_txt})",
      .envir = environment()
    )

    # ---- CONTENT
    cli::cli_text("{ch$l}{ch$b}{.h content}", .envir = environment())
    cli::cli_text(" {ch$v}{ch$j}{ch$b}design rows:     {n_design}", .envir = environment())
    cli::cli_text(" {ch$v}{ch$j}{ch$b}runs:            {n_runs}", .envir = environment())
    cli::cli_text(" {ch$v}{ch$l}{ch$b}solutions:       {n_solutions}", .envir = environment())

    # ---- RUN SUMMARY
    cli::cli_text("{ch$l}{ch$b}{.h run summary}", .envir = environment())
    cli::cli_text(" {ch$v}{ch$j}{ch$b}statuses:        {status_txt}", .envir = environment())
    cli::cli_text(" {ch$v}{ch$j}{ch$b}runtime:         {runtime_txt}", .envir = environment())
    cli::cli_text(" {ch$v}{ch$j}{ch$b}gap:             {gap_txt}", .envir = environment())
    cli::cli_text(" {ch$v}{ch$j}{ch$b}design cols:     {design_txt}", .envir = environment())
    cli::cli_text(" {ch$v}{ch$l}{ch$b}value cols:      {value_txt}", .envir = environment())

    info_sym <- cli::symbol$info
    if (is.function(info_sym)) info_sym <- info_sym()
    cli::cli_text(
      cli::col_grey(
        paste0(
          "# ", info_sym,
          " Use {.code x$solution$runs}, {.code x$solution$design}, ",
          "and {.code x$solution$solutions[[i]]} to inspect details."
        )
      )
    )

    cli::cli_end(div_id)
    invisible(TRUE)
  },

  show = function(self) self$print(),

  repr = function(self) {
    method_name <- .pa_solutionset_method_label(self$method)
    runs <- self$solution$runs %||% NULL
    n_runs <- if (inherits(runs, "data.frame")) nrow(runs) else 0L
    paste0("<SolutionSet> method=", method_name, ", runs=", n_runs)
  },

  getMethod = function(self) self$method,
  getDesign = function(self) self$solution$design %||% NULL,
  getRuns = function(self) self$solution$runs %||% NULL,
  getSolutions = function(self) self$solution$solutions %||% list()
)
