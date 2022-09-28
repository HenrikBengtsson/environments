#' Prune and call a function with a set arguments
#'
#' @param fcn A \code{\link[base:function]{function}}.
#'
#' @param args A \code{\link[base:list]{list}} of arguments passed to
#' the function `fcn`.
#'
#' @param envir An \code{\link[base:environment]{environment}} within
#' which to evaluate the call.
#'
#' @param prune If TRUE, the environment stack of `fcn` is pruned.
#'
#' @return
#' Return the value of function call.
#'
#' @example incl/do_call_1.R
#'
#' @seealso
#' [base::do.call()]
#'
#' @export
do_call <- function(fcn, args = list(), envir = parent.frame(), prune = FALSE) {
  fcn_name <- as.character(substitute(fcn))
  
  if (prune) {
    ## FIXME: Does not always pick up the right function /HB 2022-05-26
    search <- list(
      find_object_by_value(fcn, from = envir, first = FALSE)$envir
    )
    fcn <- prune_fcn(fcn, search = search)
    
    ## Important: We must drop attribute 'prune_undo' before exporting object,
    ## otherwise it will carry the pruned environment as cargo
    fcn_undo <- attr(fcn, "prune_undo")
    attr(fcn, "prune_undo") <- NULL
    
    on.exit(fcn_undo(), add = TRUE)
  }

  do.call(fcn, args = args, envir = envir)
}
