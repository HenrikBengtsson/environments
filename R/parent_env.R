#' @param n The generation of parent environment to get.
#' If `n = 0`, then `envir` is returned.
#' If `n = 1` (default), then `parent.env(envir)` is returned.
#' If `n = 2`, then `parent.env(parent.env(envir))` is returned,
#' and so on, until `emptyenv()` is returned.
#'
#' @return
#' `parent_env()` returns an \code{\link[base:environment]{environment}}.
#'
#' @examples
#' parent_env()
#'
#' ns <- getNamespace("stats")
#' print(ns)
#' parent_env(ns, n = 0)    ## same as 'ns'
#' parent_env(ns, n = 1)    ## default
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
#' environment(f)
#' parent_env(f, n = 0)     ## same as environment(f)
#' parent_env(f, n = 1)
#' parent_env(f, n = 2)
#' parent_env(f, n = 3)
#'
#' @rdname parent_envs
#' @export
parent_env <- function(envir = parent.frame(), n = 1L) {
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
