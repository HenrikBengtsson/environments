# Illustrate how free variables are located

#' Find all parent environment of an environment
#'
#' @param envir An [base::environment].
#'
#' @return
#' A named list of environments, where the first element is `envir`
#' and the last is `emptyenv()`.
#'
#' @examples
#' parent_envs(emptyenv())
#' parent_envs(baseenv())
#' parent_envs(globalenv())
#' parent_envs(new.env(parent = baseenv()))
#'
#' @export
parent_envs <- function(envir) {
  envirs <- list(envir)
  names <- environmentName(envir)
  while (!identical(envir, emptyenv())) {
    envir <- parent.env(envir)
    envirs <- c(envirs, envir)
    names <- c(names, environmentName(envir))
  }
  names(envirs) <- names
  envirs
}
