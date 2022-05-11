#' Find the environment where an object exists
#'
#' @param name The name of the object to locate.
#'
#' @param mode The [base::mode] of the object to locate.
#'
#' @param from An [base::environment] to start search from.
#'
#' @return An [base::environment], or NULL.
#'
#' @details
#' The object is looked for in environment `from`. If it is found there,
#' then `from` is returned.  If not found there, the parent environment
#' of `from` is searched, and so on, until the "empty" environment
#' ([base::emptyenv()]) is reached. In such cases, no matching object
#' could be found and `NULL` is returned.
#'
#' @examples
#' find_object("pi")
#' find_object("pi", mode = "character")   ## non-existing
#' find_object("rnorm", mode = "function")
#'
#' f <- local({
#'   a <- 42
#'   b <- 3.14
#'   function() a+b
#' })
#' env <- find_object("a", from = environment(f))
#' utils::ls.str(env)
#' 
#' f <- local({
#'   a <- 42
#'   local({
#'     b <- 3.14
#'     function() a+b
#'   })
#' })
#' env_a <- find_object("a", from = environment(f))
#' utils::ls.str(env_a)
#' env_b <- find_object("b", from = environment(f))
#' utils::ls.str(env_b)
#' stopifnot(identical(parent.env(env_b), env_a))
#' 
#' @export
find_object <- function(name, mode = "any", from = parent.frame()) {
  stopifnot(inherits(from, "environment"))
  envir <- from
  while (!identical(envir, emptyenv())) {
    if (exists(name, mode = mode, envir = envir, inherits = FALSE)) {
      return(envir)
    }
    envir <- parent.env(envir)
  }
  NULL
}
