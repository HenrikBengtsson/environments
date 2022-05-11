#' Find all parent environments of an environment
#'
#' @param envir An \code{\link[base:environment]{environment}}.
#'
#' @return
#' A named list of \code{\link[base:environment]{environment}}s, where
#' the first element is `envir` and the last is
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
parent_envs <- function(envir) {
  stopifnot(inherits(envir, "environment"))
  envirs <- list(envir)
  names <- environment_name(envir)
  while (!identical(envir, emptyenv())) {
    envir <- parent.env(envir)
    envirs <- c(envirs, envir)
    names <- c(names, environment_name(envir))
  }
  names(envirs) <- names
  envirs
}
