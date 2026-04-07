#' @include internal.R
#'
#' @title Add area constraint
#'
#' @description
#' Add a total selected area constraint to a planning problem.
#'
#' This function stores an area constraint in the \code{Problem} object so that
#' it can be incorporated later by the model builder when the optimization model
#' is assembled.
#'
#' @details
#' Let \eqn{\mathcal{P}} denote the set of planning units and let
#' \eqn{a_i \ge 0} be the area associated with planning unit \eqn{i \in \mathcal{P}}.
#' Let \eqn{w_i \in \{0,1\}} denote the binary variable indicating whether
#' planning unit \eqn{i} is selected by at least one decision in the model.
#'
#' Depending on \code{sense}, this function stores one of the following
#' constraints:
#'
#' If \code{sense = "min"}:
#' \deqn{
#' \sum_{i \in \mathcal{P}} a_i w_i \ge A
#' }
#'
#' If \code{sense = "max"}:
#' \deqn{
#' \sum_{i \in \mathcal{P}} a_i w_i \le A
#' }
#'
#' If \code{sense = "equal"} and \code{tolerance = 0}:
#' \deqn{
#' \sum_{i \in \mathcal{P}} a_i w_i = A
#' }
#'
#' If \code{sense = "equal"} and \code{tolerance > 0}:
#' \deqn{
#' A - \tau \le \sum_{i \in \mathcal{P}} a_i w_i \le A + \tau
#' }
#' where \eqn{\tau} is the value supplied through \code{tolerance}.
#'
#' Areas are obtained from \code{x$data$pu}. If \code{area_col} is provided, that
#' column is used. Otherwise, the model builder will later determine the default
#' area source according to the internal rules of the package. The value of
#' \code{area_unit} indicates the unit in which \code{area} and
#' \code{tolerance} are expressed and therefore how the stored threshold should
#' be interpreted.
#'
#' This function only stores the constraint specification in
#' \code{x$data$constraints$area}; it does not validate the feasibility of the
#' threshold against the available planning units at this stage.
#'
#' At most one area constraint can be stored in a \code{Problem} object. If one
#' already exists, this function raises an error.
#'
#' @param x A \code{Problem} object.
#'
#' @param area Numeric scalar greater than or equal to zero. Target value for the
#'   total selected area.
#'
#' @param sense Character string indicating the type of area constraint. Must be
#'   one of \code{"min"}, \code{"max"}, or \code{"equal"}.
#'
#' @param tolerance Numeric scalar greater than or equal to zero. Only used when
#'   \code{sense = "equal"}. In that case, the equality is interpreted as a band
#'   around \code{area} with half-width \code{tolerance}. Ignored otherwise.
#'
#' @param area_col Optional character string giving the name of the area column
#'   in \code{x$data$pu}. If \code{NULL}, the area source is resolved later by
#'   the model builder.
#'
#' @param area_unit Character string indicating the unit of \code{area}.
#'   Must be one of \code{"m2"}, \code{"ha"}, or \code{"km2"}.
#'
#' @param name Character string used as the label of the stored linear
#'   constraint when it is later added to the optimization model.
#'
#' @return An updated \code{Problem} object with a stored area constraint in
#'   \code{x$data$constraints$area}.
#'
#' @seealso
#' \code{\link{create_problem}}
#'
#' @examples
#' pu <- data.frame(
#'   id = 1:4,
#'   cost = c(2, 3, 1, 4),
#'   area_ha = c(10, 15, 8, 20)
#' )
#'
#' features <- data.frame(
#'   id = 1:2,
#'   name = c("sp1", "sp2")
#' )
#'
#' dist_features <- data.frame(
#'   pu = c(1, 1, 2, 3, 4, 4),
#'   feature = c(1, 2, 1, 2, 1, 2),
#'   amount = c(1, 2, 1, 3, 2, 1)
#' )
#'
#' p <- create_problem(
#'   pu = pu,
#'   features = features,
#'   dist_features = dist_features
#' )
#'
#' p <- add_constraint_area(
#'   x = p,
#'   area = 25,
#'   sense = "min",
#'   area_col = "area_ha",
#'   area_unit = "ha"
#' )
#'
#' p$data$constraints$area
#'
#' @export
add_constraint_area <- function(x,
                                area,
                                sense,
                                tolerance = 0,
                                area_col = NULL,
                                area_unit = c("m2", "ha", "km2"),
                                name = "area") {

  stopifnot(inherits(x, "Problem"))

  if (missing(sense) || is.null(sense)) {
    stop(
      "`sense` must be explicitly provided. Use one of: 'min', 'max', or 'equal'.",
      call. = FALSE
    )
  }

  sense <- match.arg(sense, c("min", "max", "equal"))

  area_unit <- match.arg(area_unit)

  assertthat::assert_that(
    assertthat::is.number(area),
    is.finite(area),
    area >= 0
  )

  assertthat::assert_that(
    assertthat::is.number(tolerance),
    is.finite(tolerance),
    tolerance >= 0
  )

  if (!is.null(area_col)) {
    assertthat::assert_that(
      is.character(area_col),
      length(area_col) == 1,
      !is.na(area_col),
      nzchar(area_col)
    )
  }

  if (!is.character(name) || length(name) != 1 || is.na(name) || !nzchar(name)) {
    stop("`name` must be a non-empty character string.", call. = FALSE)
  }

  if (sense != "equal" && tolerance > 0) {
    warning(
      "`tolerance` is only used when sense = 'equal'. It will be ignored.",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  x <- .pa_clone_data(x)

  if (is.null(x$data$constraints) || !is.list(x$data$constraints)) {
    x$data$constraints <- list()
  }

  if (!is.null(x$data$constraints$area)) {
    stop(
      "An area constraint already exists. Remove or replace it explicitly before adding a new one.",
      call. = FALSE
    )
  }

  x$data$constraints$area <- list(
    type = "area",
    sense = sense,
    value = as.numeric(area),
    tolerance = if (sense == "equal") as.numeric(tolerance) else 0,
    unit = area_unit,
    area_col = area_col,
    name = as.character(name)[1]
  )

  if (!is.null(x$data$model_ptr)) {
    x$data$meta <- x$data$meta %||% list()
    x$data$meta$model_dirty <- TRUE
  }

  x
}
