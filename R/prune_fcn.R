#' Replace one of the parent environments of a function with a fresh environment
#'
#' @param object A \code{\link[base:function]{function}} or a
#' \code{\link[base:tilde]{formula}}) that should be updated.
#'
#' @param replace A \code{\link[base:environment]{environment}} to be
#' replaced environment with a pruned version.
#' It is also possible to specify a list of alternative environments.
#'
#' @param populate An optional, named list of objects to be assigned to
#' the fresh environment replacing environment `replace`.
#'
#' @return A pruned version of `object`.
#'
#' @examples
#' a <- 42
#' f <- local(function() a)
#' 
#' f_envs <- parent_envs(environment(f), until = environment(), extra = 1L)
#' names(f_envs)
#' f()
#' 
#' g <- prune_fcn(f, replace = environment(), populate = list(a = 13))
#' g_envs <- parent_envs(environment(g), until = list(environment(), parent.env(environment())))
#' names(g_envs)
#' g()
#' 
#' ## However, we also changed f() here!
#' f()
#' f_envs <- parent_envs(environment(f), until = list(environment(), parent.env(environment())))
#' stopifnot(identical(f_envs, g_envs))
#'
#' @export
prune_fcn <- function(object, replace = globalenv(), populate = NULL) {
  envir <- environment(object)
  if (!inherits(envir, "environment")) {
    stop(sprintf("Argument 'object' does not have an environment: %s", mode(object)))
  }

  if (!is.list(replace)) replace <- list(replace)
  for (env in replace) stopifnot(inherits(env, "environment"))

  if (!is.null(populate)) {
    stopifnot(is.list(populate), !is.null(names(populate)))
  }

  until <- replace
  envirs <- parent_envs(envir, until = until)
  
  ## Nothing to do?
  n <- length(envirs)
  if (n == 1L) return(object)

  top <- envirs[[n]]
  if (identical(top, emptyenv())) {
    stop(sprintf("Cannot prune %s, because the top parent environment is the empty environment: %s", mode(object)[1]), paste(names(envirs), collapse = " -> "))

  }
  
  pruned_env <- new.env(parent = parent.env(top))
  if (length(populate) > 0L) {
    for (name in names(populate)) {
      assign(name, populate[[name]], envir = pruned_env)
    }
  }
  last <- envirs[[n - 1L]]
  parent.env(last) <- pruned_env

  environment(object) <- envir
  
  object
}
