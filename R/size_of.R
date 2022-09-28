#' Get the serialization size of an object
#'
#' @param object An R object whose size should be calculated.
#'
#' @return The number of bytes the object consumes if serialized.
#' The value inherits class `object_size`.
#'
#' @example incl/size_of.R
#' 
#' @export
size_of <- function(object) {
  con <- rawConnection(raw(), open = "w")
  on.exit(close(con))
  suppressWarnings(serialize(object, connection = con))
  size <- length(rawConnectionValue(con))
  class(size) <- c("object_size", class(size))
  size
}
