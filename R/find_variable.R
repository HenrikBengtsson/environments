#' @export
find_variable <- function(name, mode = "any", from = parent.frame()) {
  stopifnot(inherits(from, "environment"))
  envir <- from
  while (!identical(envir, emptyenv())) {
    if (exists(name, mode = mode, envir = envir, inherits = FALSE)) {
      return(envir)
    }
    envir <- parent.env(envir)
  }
  NULL
}
