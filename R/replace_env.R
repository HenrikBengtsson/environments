#' Replace one of the parent environments with another
#'
#' @param envir An \code{\link[base:environment]{environment}}.
#'
#' @param search A \code{\link[base:environment]{environment}} to be
#' replaced environment with a pruned version.
#' It is also possible to specify a list of alternative environments.
#'
#' @param replace A \code{\link[base:environment]{environment}}.
#'
#' @param same_parent If TRUE, the parent environment of `replace` is
#' set to the parent environment of the replaced "search" environment.
#'
#' @return Invisibly, a named list parent environments until the one
#' that was replaced.
#'
#' @examples
#' a <- 42
#' f <- local(function() a)
#' 
#' f_envs <- parent_envs(environment(f), until = environment(), extra = 1L)
#' names(f_envs)
#' y <- f()
#' y
#'
#' new <- as.environment(list(a = 13, pi = 3.14))
#' old_parents <- replace_env(environment(f), search = environment(), replace = new)
#' names(old_parents)
#'
#' f_envs <- parent_envs(environment(f), until = list(environment(), parent.env(environment())))
#' names(f_envs)
#'
#' ## Note that f() will now see a = 13 in the replaced environment
#' ## rather than a = 42 in the calling environment
#' z <- f()
#' z
#'
#' @export
replace_env <- function(envir, search, replace, same_parent = TRUE) {
  stopifnot(inherits(envir, "environment"))
  stopifnot(inherits(replace, "environment"))
  stopifnot(is.logical(same_parent), length(same_parent) == 1L, !is.na(same_parent))

  if (!is.list(search)) search <- list(search)
  for (env in search) stopifnot(inherits(env, "environment"))

  envirs <- parent_envs(envir, until = search)
  
  ## Nothing to do?
  n <- length(envirs)
  if (n == 1L) return(envirs)

  last <- envirs[[n]]
  if (same_parent) {
    if (identical(last, emptyenv())) {
      ## Special case: replace the empty environment
      parent.env(replace) <- emptyenv()
    } else {
      parent.env(replace) <- parent.env(last)
    }
  }
  
  child <- envirs[[n - 1L]]
  parent.env(child) <- replace

  invisible(envirs)
}
