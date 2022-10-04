#' Find the environment where an object exists
#'
#' @inheritParams find_object_by_name
#' @inheritParams find_object_by_value
#'
#' @return An \code{\link[base:environment]{environment}}, or NULL.
#'
#' @example incl/find_object_1.R
#'
#' @example incl/find_object_2.R
#'
#' @export
find_object <- function(name = NULL, mode = "any", value = NULL, from = parent.frame(), until = emptyenv(), which = c("first", "last", "all")) {
  if (is.null(name) && is.null(value)) {
    stop("Either argument 'name' or 'value' must be specified, i.e. non-NULL")
  } else if (!is.null(name) && !is.null(value)) {
    stop("Both arguments 'name' and 'value' cannot be specified, i.e. non-NULL")
  }

  if (!is.null(name)) {
    find_object_by_name(name, mode = mode, from = from, until = until)
  } else if (!is.null(value)) {
    find_object_by_value(value, from = from, until = until, which = which)
  }
}
