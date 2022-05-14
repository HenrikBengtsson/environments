#' The set of environments searched to locate an object
#'
#' @inheritParams parent_envs
#'
#' @return A named list of environments.
#' The first environment is the calling environment, and
#' the last is the empty environment.
#'
#' @example incl/search_path_1.R
#'
#' @export
search_path <- function(until = emptyenv(), envir = parent.frame()) {
  envir <- list(envir)
  names(envir) <- environment_name(envir[[1]])
  c(envir, parent_envs(environment(sys.function(which = 0)), until = until))
}
