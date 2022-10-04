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
#' @return An \code{\link[base:environment]{environment}}, or NULL.
#'
#' @details
#' The object is looked for in environment `from`. If it is found there,
#' then `from` is returned.  If not found there, the parent environment
#' of `from` is searched, and so on, until an environment in `until`, or
#' the "empty" environment (\code{\link[base:emptyenv]{emptyenv()}}) is
#' reached. In such cases, no matching object could be found and NULL is
#' returned.
#'
#' `find_object_by_name()` is how [base::exists()], [base::get()], and
#' [base::assign()] locate an object based on its name and mode.
#' For example, `exists(name) == !is.null(find_object_by_name(name))`. Similarly,
#' `object <- get(name)` is the same as:
#'
#' ```r
#' envir <- find_object_by_name(name)
#' if (is.null(envir)) stop(sprintf("Object %s not found", sQuote(name)))
#' object <- get(name, envir = envir, inherits = FALSE)
#' ```
#'
#' @example incl/find_object_by_name_1.R
#' 
#' @export
find_object_by_name <- function(name, mode = "any", from = parent.frame(), until = emptyenv()) {
  if (inherits(from, "environment")) {
    envir <- from
  } else {
    envir <- environment(from)
    if (!inherits(envir, "environment")) {
      stop(sprintf("Argument 'from' does not specify an environment or an object with an environment: %s", mode(from)))
    }
  }

  if (!is.list(until)) until <- list(until)
  for (env in until) stopifnot(inherits(env, "environment"))

  ## Make sure there's always an empty environment at the end
  until <- c(until, list(emptyenv()))

  in_until <- function(envir) {
    for (env in until) {
      if (identical(envir, env)) return(TRUE)
    }
    FALSE
  }
  
  while (!in_until(envir)) {
    if (exists(name, mode = mode, envir = envir, inherits = FALSE)) {
      return(envir)
    }
    envir <- parent.env(envir)
  }
  NULL
}
