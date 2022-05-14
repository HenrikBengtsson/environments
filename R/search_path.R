#' The set of environments searched to locate an object
#'
#' @inheritParams parent_envs
#'
#' @param parent The parent generation to query for the parent frame
#' and the function.
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
search_path <- function(parent = 1L, until = emptyenv()) {
  stopifnot(length(parent) == 1L, is.numeric(parent), !is.na(parent), parent >= 1)
  envir <- list(parent.frame(n = parent))
  names(envir) <- environment_name(envir[[1]])
  c(envir, parent_envs(environment(sys.function(which = -parent)), until = until))
}
