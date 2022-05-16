#' Remove unused objects from a function's parent environments
#'
#' @inheritParams replace_env
#'
#' @param fcn A \code{\link[base:function]{function}}.
#'
#' @param globals A named list of variables to be part of the injected
#' environment.
#'
#' @return A pruned version of `fcn`, with `prune_undo` attribute holding
#' an "undo" function. _WARNING: Make sure to copy this attribute and then
#' remove it before exporting the function to an external process._
#'
#' @example incl/prune_fcn_1.R
#' 
#' @export
prune_fcn <- function(fcn, search = locate_object(fcn, from = parent.frame(), first = FALSE)$envir, globals = get_globals(fcn)) {
  stopifnot(is.function(fcn))
  if (!is.list(search)) {
    search <- list(search)
    names(search) <- environment_name(search[[1]])
  }
  for (env in search) stopifnot(inherits(env, "environment"))
  if (is.null(globals)) globals <- list()
  stopifnot(is.list(globals), !is.null(names(globals)))
  
  fcn_env <- environment(fcn)
  new <- as.environment(globals)
  old <- replace_env(fcn_env, search = search, replace = new)

  ## Special case: Nothing replaced?
  if (identical(old, fcn_env)) {
    parent.env(new) <- parent.env(fcn_env)
    environment(fcn) <- new
    prune_undo <- function() {
      environment(fcn) <- old
      fcn
    }
  } else {
    prune_undo <- function() {
      replace_env(fcn_env, search = new, replace = old)
      fcn
    }
  }  

  ## Provide a function that undoes the pruning
  ## IMPORTANT: This needs to be dropped before exporting
  attr(fcn, "prune_undo") <- prune_undo
  
  fcn
}
