#' Get the environment of an object
#'
#' @param envir An R object.
#'
#' @return A non-empty character string.
#'
#' @details
#' Returns an \code{\link[base:environment]{environment}}, if `envir` is
#' an environment, a function, or a formula.  In all other cases, NULL is
#' returned.
#'
#' @examples
#' ## An environment
#' environment_of(globalenv())    ## <environment: R_GlobalEnv>
#'
#' ## A primitive function (always in the 'base' namespace)
#' environment_of(abs)            ## <environment: namespace:base>
#'
#' ## A package function
#' environment_of(mean)           ## <environment: namespace:base>
#'
#' ## Another package function
#' environment_of(stats::median)  ## <environment: namespace:stats>
#'
#' ## A formula
#' environment_of(y ~ x)          ## <environment: R_GlobalEnv>
#'
#' ## A non-function object
#' environment_of(pi)             ## NULL
#' 
#' @export
environment_of <- function(envir) {
  if (inherits(envir, "environment")) return(envir)
  
  e <- environment(envir)
  if (!is.null(e)) return(e)
     
  ## Special case: primitive functions are all in the 'base' namespace
  if (is.primitive(envir)) return(.BaseNamespaceEnv)

  NULL
}
