#' Replace top parent environment of a function with a pruned environment
#'
#' @inheritParams parent_envs
#'
#' @param object A \code{\link[base:function]{function}} or a
#' \code{\link[base:tilde]{formula}}) whose top parent environment
#' should be replaced with a pruned environment.
#'
#' @return A pruned version of `object`.
#'
#' @export
prune_function <- function(object, until = globalenv()) {
  envir <- environment(object)
  if (!inherits(envir, "environment")) {
    stop(sprintf("Argument 'object' does not have an environment: %s", mode(object)))
  }
  stopifnot(inherits(until, "environment"))

  envirs <- parent_envs(envir, until = until)
  
  ## Nothing to do?
  if (length(envirs) == 1L) return(object)

  last <- envirs[[length(envirs) - 1L]]
  pruned_env <- new.env(parent = emptyenv())
  parent.env(last) <- pruned_env

  environment(object) <- envir
  
  envirs
}
