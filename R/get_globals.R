#' Get global variables on a function
#'
#' @param fcn The \code{\link[base:function]{function}}.
#'
#' @param must_exist If TRUE, an error is produced if one of the prospect
#' globals cannot be located.
#'
#' @return A named list of globals.
#'
#' @example incl/get_globals_1.R
#' @example incl/get_globals_2.R
#'
#' @importFrom globals cleanup globalsOf
#' @export
get_globals <- function(fcn, must_exist = FALSE) {
  stopifnot(is.function(fcn))

  ## Identify globals
  globals <- globalsOf(fcn, envir = environment(fcn), locals = TRUE, must_exist = must_exist)
  globals <- cleanup(globals)
  
  globals
}
