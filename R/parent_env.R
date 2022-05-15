#' Get ancestral environments of an environment
#'
#' @param envir An \code{\link[base:environment]{environment}}
#' or an object with an environment, e.g. a function or a formula.
#'
#' @param n The generation of parent environment to get.
#' If `n = 0`, then `envir` is returned.
#' If `n = 1`, then `parent.env(envir)` is returned.
#' If `n = 2`, then `parent.env(parent.env(envir))` is returned,
#' and so on.
#'
#' @return
#' An \code{\link[base:environment]{environment}}.
#'
#' @examples
#' ns <- getNamespace("stats")
#' parent_env(ns, n = 0)
#' parent_env(ns, n = 1)
#' parent_env(ns, n = 2)
#' parent_env(ns, n = 3)
#' parent_env(ns, n = Inf)  ## always emptyenv()
#'
#' f <- local({
#'   a <- 42
#'   local({
#'     pi <- 3.14
#'     function() pi * a
#'   })
#' })
#' parent_env(f, n = 1)
#' parent_env(f, n = 2)
#' parent_env(f, n = 3)
#'
#' @export
parent_env <- function(envir, n = 1L) {
  if (!inherits(envir, "environment")) {
     e <- environment(envir)
     if (is.null(e)) {
       stop(sprintf("Argument 'envir' must be an environment or an object with an environment: %s", mode(envir)))
     }
     envir <- e
  }
  stopifnot(is.numeric(n), length(n) == 1L, !is.na(n), n >= 0L)

  while (n > 0L && !identical(envir, emptyenv())) {
    envir <- parent.env(envir)
    n <- n - 1L
  }
  
  envir
}
