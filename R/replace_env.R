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
#' @example incl/replace_env.R
#' @example incl/replace_env_2.R
#'
#' @export
replace_env <- function(envir, search, replace, update_parent = TRUE) {
  stopifnot(inherits(envir, "environment"))
  if (!is.list(search)) {
    search <- list(search)
    names(search) <- environment_name(search[[1]])
  }
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
    stop(sprintf("Cannot replace environment. None of the environments specified in 'search' (%s) are among the parent environments of 'envir': %s", paste(sQuote(names(search)), collapse = ", "), paste(sQuote(names(envirs)), collapse = ", ")))
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
