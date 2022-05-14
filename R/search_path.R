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
#' @details
#' This is the same environment search path that is used by
#' `exist(name, inherits = TRUE)` and `get(name, inherits = TRUE)`.
#'
#' @export
search_path <- function(until = emptyenv()) {
  envir <- list(parent.frame())
  names(envir) <- environment_name(envir[[1]])
  c(envir, parent_envs(environment(sys.function(which = -1L)), until = until))
}
