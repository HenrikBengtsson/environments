#' @return
#' `top_env()` returns the top parent
#' \code{\link[base:environment]{environment}}, which
#' is either the `until` environment or the empty environment.
#'
#' @examples
#' a <- 42
#' pi <- 3.14
#' f <- function() pi * a
#' env <- top_env(f)
#' print(env)
#' #stopifnot(identical(env, environment()))
#'
#' f <- local({
#'   a <- 42
#'   local({
#'     pi <- 3.14
#'     function() pi * a
#'   })
#' })
#' env <- top_env(f)
#' print(env)
#' #stopifnot(identical(env, environment()))
#'
#' make_fcn <- function() {
#'   a <- 42
#'   pi <- 3.14
#'   function() pi * a
#' }
#' f <- make_fcn()
#' env <- top_env(f)
#' print(env)
#' #stopifnot(identical(env, environment()))
#'
#' @rdname parent_envs
#' @export
top_env <- function(envir, until = globalenv()) {
  envir <- environment_of(envir0 <- envir)
  if (!inherits(envir, "environment")) {
    stop(sprintf("Argument 'envir' must be an environment or an object with an environment: %s", mode(envir0)))
  }
  stopifnot(inherits(until, "environment"))

  while (!identical(envir, until) && !identical(envir, emptyenv())) {
    envir <- parent.env(envir)
  }
  
  envir
}
