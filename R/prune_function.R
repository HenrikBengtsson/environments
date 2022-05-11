#' Replace top parent environment of a function with a pruned environment
#'
#' @inheritParams parent_envs
#'
#' @param object A \code{\link[base:function]{function}} or a
#' \code{\link[base:tilde]{formula}}) whose top parent environment
#' should be replaced with a pruned environment.
#'
#' @param populate An optional, named list of objects to be assigned to
#' the top parent environment.
#'
#' @return A pruned version of `object`.
#'
#' @examples
#' a <- 42
#' f <- local(function() a)
#' 
#' f_envs <- parent_envs(environment(f), until = environment())
#' names(f_envs)
#' f()
#' 
#' g <- prune_function(f, until = environment(), populate = list(a = 13))
#' g_envs <- parent_envs(environment(g), until = environment())
#' names(g_envs)
#' g()
#' 
#' ## However, we also changed f() here!
#' f()
#' f_envs <- parent_envs(environment(f), until = environment())
#' stopifnot(identical(f_envs, g_envs))
#'
#' @export
prune_function <- function(object, populate = NULL, until = globalenv()) {
  envir <- environment(object)
  if (!inherits(envir, "environment")) {
    stop(sprintf("Argument 'object' does not have an environment: %s", mode(object)))
  }
  stopifnot(inherits(until, "environment"))
  if (!is.null(populate)) {
    stopifnot(is.list(populate), !is.null(names(populate)))
  }

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
