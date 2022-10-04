#' Find the environment where an object exists
#'
#' @inheritParams parent_envs
#'
#' @param name The name of the object to locate.
#'
#' @param mode The \code{\link[base:mode]{mode}} of the object to locate.
#'
#' @param from An \code{\link[base:environment]{environment}}, or an object
#' with an environment (e.g. a \code{\link[base:function]{function}} and a
#' \code{\link[base:tilde]{formula}}), to start search from.
#'
#' @param value The R object whose location should be identified.
#'
#' @param which If `"first"` or `"last"`, then the first or the last
#' occurance of `object` among the parent frames is identified and returned.
#' If `"all"`, then all occurances are returned.
#'
#' @return
#' A named list with elements `name` and `envir`, where `name` is the
#' name of the located object in environment `envir`.
#' If no object could be located, then NULL is returned.
#' If more than one matching object could be located, then a list of
#' all matching (name, environment) lists is returned if `which = "all"`.
#' If `which = "first"`, then the first match is returned,
#' and if `which = "last"`, then the last match is returned.
#'
#' @section Environments searched:
#' A matching object is looked for in environment `from`. If it is found
#' there, then `from` is returned.  If not found there, the parent
#' environment of `from` is searched, and so on, until an environment in
#' `until`, or the "empty" environment
#' (\code{\link[base:emptyenv]{emptyenv()}}) is reached. In such cases,
#' no matching object could be found and NULL is returned.
#'
#' @section Find an object by its name and mode:
#' `find_object()` with arguments `name` and `mode` locates an object
#' with name `name` and mode `mode` in one of the environments searched.
#' This is how [base::exists()], [base::get()], and [base::assign()] locate
#' an object based on its name and mode.
#' For example, `exists(name) == !is.null(find_object(name = name))`.
#" Similarly, `object <- get(name)` is the same as:
#'
#' ```r
#' envir <- find_object(name = name)
#' if (is.null(envir)) stop(sprintf("Object %s not found", sQuote(name)))
#' object <- get(name, envir = envir, inherits = FALSE)
#' ```
#'
#' @section Find an object by its value:
#' `find_object()` with arguments `value` locates an object of any name
#' with value `value` in one of the environments searched.
#'
#' @example incl/find_object_1.R
#' @example incl/find_object_2.R
#'
#' @export
find_object <- function(name = NULL, mode = "any", value = NULL, from = parent.frame(), until = emptyenv(), which = c("first", "last", "all")) {
  if (is.null(name) && is.null(value)) {
    stop("Either argument 'name' or 'value' must be specified, i.e. non-NULL")
  } else if (!is.null(name) && !is.null(value)) {
    stop("Both arguments 'name' and 'value' cannot be specified, i.e. non-NULL")
  }

  which <- match.arg(which)

  if (!is.null(name)) {
    find_object_by_name(name, mode = mode, from = from, until = until, which = which)
  } else if (!is.null(value)) {
    find_object_by_value(value, from = from, until = until, which = which)
  }
}
