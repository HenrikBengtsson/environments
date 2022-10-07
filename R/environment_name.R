#' Get the name of an environment
#'
#' @param envir An \code{\link[base:environment]{environment}}.
#'
#' @return A non-empty character string.
#'
#' @details
#' This function returns what
#' \code{\link[base:environmentName]{environmentName(envir)}} with a few
#' exceptions.  One exception is that it, instead of `"base"`, returns
#' `"package:base"` for `baseenv()`, which is the same environment as
#' `pos.to.env(grep("package:base", search()))`.  This distinguishes
#' from `"base"` returned for `getNamespace("base")`, which is _not_
#' the same environment.  Another exception is when `environmentName()`
#' returns an empty string, e.g. when `envir <- new.env()`.  In such cases,
#' it returns the hexadecimal string that is outputted by `print(envir)`.
#'
#' @examples
#' environment_name(emptyenv())             ## "R_EmptyEnv"
#'
#' environment_name(globalenv())            ## "R_GlobalEnv"
#'
#' environment_name(baseenv())              ## "package:base"
#'
#' environment_name(.BaseNamespaceEnv)      ## "base"
#'
#' environment_name(getNamespace("base"))   ## "base"
#'
#' environment_name(getNamespace("utils"))  ## "utils"
#'
#' environment_name(new.env())              ## e.g. "0x55ff679a1ce0"
#' 
#' 
#' @importFrom utils capture.output
#' @export
environment_name <- function(envir) {
  stopifnot(inherits(envir, "environment"))
  name <- environmentName(envir)
  
  ## Special case: environmentName() returns "base" for both
  ## baseenv() and getNamespace("base"), although the two are
  ## not the same environment.
  if (name == "base") {
    if (identical(envir, baseenv())) {
      name <- "package:base"
    }
  }
  
  if (!nzchar(name)) {
    ## Here we call print.default(), instead of generic print(), to
    ## avoid the risk of someone defining a print.environment().
    name <- capture.output(print.default(envir))
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
