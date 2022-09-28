#' Find the environment where an object exists
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
#' of `from` is searched, and so on, until the "empty" environment
#' (\code{\link[base:emptyenv]{emptyenv()}}) is reached. In such cases,
#' no matching object could be found and NULL is returned.
#'
#' `find_object()` is how [base::exists()], [base::get()], and
#' [base::assign()] locate an object with `inherits = TRUE` (default).
#' For example, `exists(name) == !is.null(find_object(name))`. Similarly,
#' `object <- get(name)` is the same as:
#'
#' ```r
#' envir <- find_object(name)
#' if (is.null(envir)) stop(sprintf("Object %d not found", sQuote(name)))
#' object <- get(name, envir = envir, inherits = FALSE)
#' ```
#'
#' @examples
#' find_object("pi")
#' find_object("pi", mode = "character")   ## non-existing
#' find_object("rnorm", mode = "function")
#'
#' f <- local({
#'   a <- 42
#'   pi <- 3.14
#'   function() pi * a
#' })
#' env <- find_object("a", from = f)
#' utils::ls.str(env)
#' 
#' f <- local({
#'   a <- 42
#'   local({
#'     pi <- 3.14
#'     function() pi * a
#'   })
#' })
#' env_a <- find_object("a", from = f)
#' utils::ls.str(env_a)
#' env_pi <- find_object("pi", from = f)
#' utils::ls.str(env_pi)
#' stopifnot(
#'   identical(environment(f), env_pi),
#'   identical(parent.env(env_pi), env_a)
#' )
#' 
#' @export
find_object <- function(name, mode = "any", from = parent.frame()) {
  if (inherits(from, "environment")) {
    envir <- from
  } else {
    envir <- environment(from)
    if (!inherits(envir, "environment")) {
      stop(sprintf("Argument 'from' does not specify an environment or an object with an environment: %s", mode(from)))
    }
  }
  while (!identical(envir, emptyenv())) {
    if (exists(name, mode = mode, envir = envir, inherits = FALSE)) {
      return(envir)
    }
    envir <- parent.env(envir)
  }
  NULL
}
