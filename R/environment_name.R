#' @importFrom utils capture.output
#' @export
environment_name <- function(envir) {
  stopifnot(inherits(envir, "environment"))
  name <- environmentName(envir)
  if (!nzchar(name)) {
    name <- capture.output(print(envir))
    name <- gsub("(^<.*[[:space:]]+|>$)", "", name)
  }
  name
}
