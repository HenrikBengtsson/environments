#' Locate the original name and location of an object
#'
#' @param object The R object whose location should be identified.
#'
#' @param from An \code{\link[base:environment]{environment}}, or an object
#' with an environment (e.g. a \code{\link[base:function]{function}} and a
#' \code{\link[base:tilde]{formula}}), to start search from.
#'
#' @param first If TRUE, the first occurance of `object` among the parent
#' frames is identified, otherwise the last.
#'
#' @return
#' A named list with elements `name` and `envir`, where `name` is the
#' name of `object` as it is named in environment `envir`, i.e.
#' `identical(envir[[name]], object)`.
#' If the object could not be located when searching from environment
#' `from`, then NULL is returned.
#'
#' @example incl/locate_object_1.R
#'
#' @export
locate_object <- function(object, from = parent.frame(), first = TRUE) {
  if (inherits(from, "environment")) {
    envir <- from
  } else {
    envir <- environment(from)
    if (!inherits(envir, "environment")) {
      stop(sprintf("Argument 'from' does not specify an environment or an object with an environment: %s", mode(from)))
    }
  }
  stopifnot(length(first) == 1L, is.logical(first), !is.na(first))
  
  mode <- mode(object) ## scan for objects of this mode

  ## WORKAROUND: R CMD check will add mockup 'F' and 'T' objects
  ## to catch misuse of 'F' instead of 'FALSE' and 'T' and 'TRUE'
  ## in examples. Here we detect if example() is running under
  ## 'R CMD check'.  If it does, we'll skip searching that
  ## environment.
  skip <- NULL
  if ("CheckExEnv" %in% search()) {
    skip <- as.environment("CheckExEnv")
  }

  res <- NULL
  while (!identical(envir, emptyenv())) {
    ## Skip?
    if (identical(envir, skip)) {
      envir <- parent.env(envir)
      next
    }
    
    ## Scan all objects in the current environment ...
    names <- ls(envir, all.names = TRUE)
    if (identical(envir, baseenv())) {
      names <- setdiff(names, ".Last.value")
    }
    for (name in names) {
      ## ... consider only those with the same mode as 'object' ...
      if (exists(name, mode = mode, envir = envir, inherits = FALSE)) {
        tmp <- get(name, mode = mode, envir = envir, inherits = FALSE)
        ## ... are the identical?
        if (identical(tmp, object, ignore.environment = FALSE, ignore.bytecode = FALSE, ignore.srcref = FALSE)) {
          res <- list(name = name, envir = envir)
          if (first) return(res)
        }
      }
    }
    envir <- parent.env(envir)
  }

  res
}

