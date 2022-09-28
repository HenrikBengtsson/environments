#' Find the top environments of a function or a formula
#'
#' The top environment is the top parent environment 
#'
#' @inheritParams parent_envs
#'
#' @param object A \code{\link[base:function]{function}} or a
#' \code{\link[base:tilde]{formula}}) whose top parent environment
#' should be found.
#'
#' @return An \code{\link[base:environment]{environment}}, which
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
#' @seealso
#' [parent_env()] and [parent_envs()].
#'
#' @export
top_env <- function(object, until = globalenv()) {
  envir <- environment(object)
  if (!inherits(envir, "environment")) {
    stop(sprintf("Argument 'object' does not have an environment: %s", mode(object)))
  }
  stopifnot(inherits(until, "environment"))

  while (!identical(envir, until) && !identical(envir, emptyenv())) {
    envir <- parent.env(envir)
  }
  
  envir
}
