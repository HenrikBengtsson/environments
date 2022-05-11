#' Replace one of the parent environments with another
#'
#' @param envir An \code{\link[base:environment]{environment}}.
#'
#' @param search A \code{\link[base:environment]{environment}},
#' among the parents of `envir`, to be replaced.
#' It is possible to specify a list of alternative environments.
#'
#' @param replace A \code{\link[base:environment]{environment}}.
#'
#' @param update_parent If TRUE, or 1L, the parent environment of
#' `replace` is set to the parent environment of the replaced
#' "search" environment. If FALSE, or 0L, it is not updated.
#' If a positive integer greater than one, then that parent
#' generation is updated, e.g. `update_parent = 2L` will update
#' the parent environment of the _parent_ of `replace`.
#'
#' @return Invisibly, the replaced environment.
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
#' old <- replace_env(environment(f), search = environment(), replace = new)
#' old
#'
#' f2_envs <- parent_envs(environment(f), until = list(environment(), parent.env(environment())))
#' names(f2_envs)
#'
#' ## Note that f() will now see a = 13 in the replaced environment
#' ## rather than a = 42 in the calling environment
#' z <- f()
#' z
#'
#' ## Undo changes
#' old2 <- replace_env(environment(f), search = new, replace = old)
#' stopifnot(identical(old2, new))
#'
#' f3_envs <- parent_envs(environment(f), until = environment(), extra = 1L)
#' stopifnot(identical(f3_envs, f_envs))
#'
#' ## f() will now see a = 42 again
#' z <- f()
#' z
#'
#' ## Example how to do the above temporarily inside a function
#' my_fcn <- function(globals = NULL) {
#'   a <- 42
#'   f <- local(function() a)
#'   if (length(globals) > 0) {
#'     new <- as.environment(globals)
#'     f_env <- environment(f)
#'     old <- replace_env(f_env, search = environment(), replace = new)
#'     on.exit(replace_env(f_env, search = new, replace = old))
#'   }
#'   f()
#' }
#'
#' my_fcn()
#' my_fcn(globals = list(a = 13))
#'
#' @export
replace_env <- function(envir, search, replace, update_parent = TRUE) {
  stopifnot(inherits(envir, "environment"))
  if (!is.list(search)) search <- list(search)
  for (env in search) stopifnot(inherits(env, "environment"))
  stopifnot(inherits(replace, "environment"))
  stopifnot(length(update_parent) == 1L, !is.na(update_parent))
  if (is.logical(update_parent)) update_parent <- as.integer(update_parent)
  stopifnot(is.numeric(update_parent), !is.na(update_parent),
            update_parent >= 0L)

  envirs <- parent_envs(envir, until = search)

  ## Assert that a match was found
  found <- FALSE
  for (env in search) {
    for (penv in envirs) {
      if (identical(penv, env)) {
        found <- TRUE
        break
      }
    }
    if (found) break
  }
  if (!found) {
    stop("None of the environments specified in 'search' are among the parent environments of 'envir'")
  }
  
  ## Nothing to do?
  n <- length(envirs)
  last <- envirs[[n]]
  if (n == 1L) return(last)

  child <- envirs[[n - 1L]]
  parent.env(child) <- replace

  ## Update parent environment of 'replace'?
  if (update_parent > 0L) {
    ## (a) Identify new parent environment
    if (identical(last, emptyenv())) {
      ## Special case: replace the empty environment
      last_parent <- emptyenv()
    } else {
      last_parent <- parent.env(last)
    }
    
    ## (b) Update parent environment of generation 'update_parent'
    count <- update_parent - 1L
    while (!identical(replace, emptyenv()) && count > 0L) {
      replace <- parent.env(replace)
      count <- count - 1L
    }
    if (identical(replace, emptyenv())) {
      stop(sprintf("Cannot replace parent generation %d of 'replace', because it either doesn't exist or is the empty environment", update_parent))
    }
    parent.env(replace) <- last_parent
  }
  
  invisible(last)
}
