#' Get the ancestral environments of an environment
#'
#' @param envir An \code{\link[base:environment]{environment}}
#' or an object with an environment, e.g. a function or a formula.
#'
#' @param until An \code{\link[base:environment]{environment}} to consider
#' the last parent environment.  If `until` is not one of the parent
#' environments, then \code{\link[base:emptyenv]{emptyenv()}} is the
#' last one.  It is also possible to specify a list of alternative
#' environments.
#'
#' @param extra Maximum number of additional parent environments to include
#' after a matching "until" environment was identified.
#'
#' @return
#' `parent_envs()` returns a named list of
#' \code{\link[base:environment]{environment}}s, where the names correspond
#' to [environment_name()] of each environment.
#' The first element is always `envir`.
#' If `extra = 0L` (default), the last is `until` or
#' \code{\link[base:emptyenv]{emptyenv()}}, which equals [top_env()].
#'
#' @details
#' Consider the following R script evaluated in the global environment:
#'
#' ```r
#' cargo <- rnorm(1e6)
#' a <- 2
#' f <- local({
#'   pi <- 3.14
#'   function() {
#'     n <- 4
#'     a * pi / n
#'   }
#' })
#' ```
#'
#' The environment of function `f()` is the local environment that
#' contains the `pi` object, i.e. `environment(f)$pi` exists.
#' The parent environment of this environment is
#' `parent.envir(environment(f))`, which can also be obtained as
#' `parent_env(f)`.  This environment contains objects `a` and `cargo`,
#' i.e. `parent_env(f)$a` and `parent_env(f)$cargo` exist.
#' If we sourced the script in the global environment, then
#' `parent_env(f)` is the global environment.
#'
#' We can retrieve these two "ancestral" environments of `f()` using
#' `parent_envs(f)`, which can be represented visually as:
#'
#' ```
#' +-----------------+
#' | parent_env(f):  | == parent_envs(f)[[2]]
#' | cargo = { 1e6 } | == parent_env(f, n = 1L)
#' | a = 2           | == top_env(f)
#' | f               | == globalenv()
#' +-----------------+
#'            ^
#'            |
#' +-----------------+
#' | environment(f): | == parent_envs(f)[[1]]
#' | pi = 3.14       | == parent_env(f, n = 0L)
#' +-----------------+
#' ```
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
#' f_envs <- parent_envs(f, until = environment())
#' names(f_envs)
#'
#' f_envs <- parent_envs(f, until = environment(), extra = 1L)
#' names(f_envs)
#'
#' @export
parent_envs <- function(envir = parent.frame(), until = globalenv(), extra = 0L) {
  if (!inherits(envir, "environment")) {
     e <- environment(envir)
     if (is.null(e)) {
       stop(sprintf("Argument 'envir' must be an environment or an object with an environment: %s", mode(envir)))
     }
     envir <- e
  }
  if (!is.list(until)) until <- list(until)
  for (env in until) stopifnot(inherits(env, "environment"))
  stopifnot(is.numeric(extra), length(extra) == 1L, !is.na(extra), extra >= 0L)

  ## Make sure there's always an empty environment at the end
  until <- c(until, list(emptyenv()))

  in_until <- function(envir) {
    for (env in until) {
      if (identical(envir, env)) return(TRUE)
    }
    FALSE
  }
  
  envirs <- list(envir)
  names <- environment_name(envir)
  while (!in_until(envir)) {
    envir <- parent.env(envir)
    envirs <- c(envirs, envir)
    names <- c(names, environment_name(envir))
  }

  while (extra > 0L && !identical(envir, emptyenv())) {
    envir <- parent.env(envir)
    envirs <- c(envirs, envir)
    names <- c(names, environment_name(envir))
    extra <- extra - 1L
  }

  names(envirs) <- names
  
  envirs
}
