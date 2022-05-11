#' Find all parent environments of an environment
#'
#' @param envir An \code{\link[base:environment]{environment}}.
#'
#' @param until An \code{\link[base:environment]{environment}} to consider
#' the last parent environment.  If `until` is not one of the parent
#' environments, then \code{\link[base:emptyenv]{emptyenv()}} is the
#' last one.
#'
#' @return
#' A named list of \code{\link[base:environment]{environment}}s, where
#' the first element is `envir` and the last is `until` or
#' \code{\link[base:emptyenv]{emptyenv()}}.
#'
#' @examples
#' parent_envs(emptyenv())
#' parent_envs(baseenv())
#' parent_envs(globalenv())
#' parent_envs(new.env(parent = baseenv()))
#'
#' f <- local({
#'   a <- 42
#'   local({
#'     pi <- 3.14
#'     function() pi * a
#'   })
#' })
#' parent_envs(environment(f))
#'
#' @export
parent_envs <- function(envir, until = emptyenv()) {
  stopifnot(inherits(envir, "environment"))
  stopifnot(inherits(until, "environment"))
  envirs <- list(envir)
  names <- environment_name(envir)
  while (!identical(envir, until) && !identical(envir, emptyenv())) {
    envir <- parent.env(envir)
    envirs <- c(envirs, envir)
    names <- c(names, environment_name(envir))
  }
  names(envirs) <- names
  envirs
}
