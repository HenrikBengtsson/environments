#' Get the name of an environment
#'
#' @param envir An \code{\link[base:environment]{environment}}.
#'
#' @return A non-empty character string.
#'
#' @details
#' This function returns what
#' \code{\link[base:environmentName]{environmentName(envir)}}, except when
#' it is empty, e.g. when `envir <- new.env()`.  In such cases, it returns
#' the hexadecimal string that is outputted by `print(envir)`.
#'
#' @examples
#' environment_name(emptyenv())
#' environment_name(globalenv())
#' environment_name(getNamespace("utils"))
#' environment_name(new.env())
#' 
#' @importFrom utils capture.output
#' @export
environment_name <- function(envir) {
  stopifnot(inherits(envir, "environment"))
  name <- environmentName(envir)
  if (!nzchar(name)) {
    name <- capture.output(print(envir))
    pattern <- "^<.*[[:space:]]+(0x[[:alnum:]]+)>$"
    res <- grep(pattern, name, value = TRUE)
    if (length(res) == 0L) {
      stop("Failed to parse environment hexadecimal string. Unexpected print(<env>) output:\n", paste(name, collapse = "\n"))
    } else if (length(res) > 1L) {
      stop("Failed to parse environment hexadecimal string. Found more than one match in print(<env>) output:\n", paste(name, collapse = "\n"))
    }
    name <- gsub(pattern, "\\1", res)
  }
  name
}
