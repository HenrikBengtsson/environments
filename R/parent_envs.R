#' Find all parent environments of an environment
#'
#' @inheritParams parent_env
#'
#' @param until A \code{\link[base:environment]{environment}} to consider
#' the last parent environment.  If `until` is not one of the parent
#' environments, then \code{\link[base:emptyenv]{emptyenv()}} is the
#' last one.  It is also possible to specify a list of alternative
#' environments.
#'
#' @param extra Maximum number of additional parent environments to include
#' after a matching "until" one was identified.
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
#' f_envs <- parent_envs(f, until = environment())
#' names(f_envs)
#'
#' f_envs <- parent_envs(f, until = environment(), extra = 1L)
#' names(f_envs)
#'
#' @export
parent_envs <- function(envir = parent.frame(), until = emptyenv(), extra = 0L) {
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
