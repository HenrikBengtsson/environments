#' Remove unused objects from a function's parent environments
#'
#' @inheritParams replace_env
#'
#' @param fcn A \code{\link[base:function]{function}}.
#'
#' @return A pruned version of `fcn`, with `prune_undo` attribute holding
#' an "undo" function. _WARNING: Make sure to copy this attribute and then
#' remove it before exporting the function to an external process._
#'
## @example incl/prune_fcn_1.R
#' 
#' @export
prune_fcn <- function(fcn, search = locate_object(fcn)$envir) {
  stopifnot(is.function(fcn))

  fcn_env <- environment(fcn)
  fcn_globals <- get_globals(fcn)
  new <- as.environment(fcn_globals)
  old <- replace_env(fcn_env, search = search, replace = new)

  ## Provide a function that undoes the pruning
  ## IMPORTANT: This needs to be dropped before exporting
  attr(fcn, "prune_undo") <- function() {
    replace_env(fcn_env, search = new, replace = old)
    fcn
  }
  
  fcn
}
