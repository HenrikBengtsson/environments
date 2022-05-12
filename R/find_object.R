#' Find the environment where an object exists
#'
#' @param name The name of the object to locate.
#'
#' @param mode The \code{\link[base:mode]{mode}} of the object to locate.
#'
#' @param from An \code{\link[base:environment]{environment}}, or an object
#' with an environment (e.g. a \code{\link[base:function]{function}} and a
#' \code{\link[base:tilde]{formula}}), to start search from.
#'
#' @return An \code{\link[base:environment]{environment}}, or NULL.
#'
#' @details
#' The object is looked for in environment `from`. If it is found there,
#' then `from` is returned.  If not found there, the parent environment
#' of `from` is searched, and so on, until the "empty" environment
#' (\code{\link[base:emptyenv]{emptyenv()}}) is reached. In such cases,
#' no matching object could be found and NULL is returned.
#'
#' @examples
#' find_object("pi")
#' find_object("pi", mode = "character")   ## non-existing
#' find_object("rnorm", mode = "function")
#'
#' f <- local({
#'   a <- 42
#'   pi <- 3.14
#'   function() pi * a
#' })
#' env <- find_object("a", from = f)
#' utils::ls.str(env)
#' 
#' f <- local({
#'   a <- 42
#'   local({
#'     pi <- 3.14
#'     function() pi * a
#'   })
#' })
#' env_a <- find_object("a", from = f)
#' utils::ls.str(env_a)
#' env_pi <- find_object("pi", from = f)
#' utils::ls.str(env_pi)
#' stopifnot(
#'   identical(environment(f), env_pi),
#'   identical(parent.env(env_pi), env_a)
#' )
#' 
#' @export
find_object <- function(name, mode = "any", from = parent.frame()) {
  if (inherits(from, "environment")) {
    envir <- from
  } else {
    envir <- environment(from)
    if (!inherits(envir, "environment")) {
      stop(sprintf("Argument 'from' does not specify an environment or an object with an environment: %s", mode(from)))
    }
  }
  while (!identical(envir, emptyenv())) {
    if (exists(name, mode = mode, envir = envir, inherits = FALSE)) {
      return(envir)
    }
    envir <- parent.env(envir)
  }
  NULL
}

#' Locate the original name and location of an object
#'
#' @param object The R object whose location should be identified.
#'
#' @param from An \code{\link[base:environment]{environment}}, or an object
#' with an environment (e.g. a \code{\link[base:function]{function}} and a
#' \code{\link[base:tilde]{formula}}), to start search from.
#'
#' @return
#' A named list with elements `name` and `envir`, where `name` is the
#' name of `object` as it is named in environment `envir`.
#' If the object could not be located when searching from environment `from`,
#' then an error is produced.
#'
#' @examples
#' locate_object(pi)
#'
#' a <- pi
#' locate_object(a)
#'
#' a <- pi
#' locate_object(a, from = parent.env(globalenv()))
#'
#' a <- local(pi)
#' locate_object(a)
#'
#' @export
locate_object <- function(object, from = parent.frame()) {
  if (inherits(from, "environment")) {
    envir <- from
  } else {
    envir <- environment(from)
    if (!inherits(envir, "environment")) {
      stop(sprintf("Argument 'from' does not specify an environment or an object with an environment: %s", mode(from)))
    }
  }

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
  
  while (!identical(envir, emptyenv())) {
    ## Skip?
    if (identical(envir, skip)) {
      envir <- parent.env(envir)
      next
    }
    
    ## Scan all objects in the current environment ...
    names <- ls(envir, all.names = TRUE)
    for (name in names) {
      ## ... consider only those with the same mode as 'object' ...
      if (exists(name, mode = mode, envir = envir, inherits = FALSE)) {
        tmp <- get(name, mode = mode, envir = envir, inherits = FALSE)
        ## ... are the identical?
        if (identical(tmp, object)) {
          return(list(name = name, envir = envir))
        }
      }
    }
    envir <- parent.env(envir)
  }
  
  stop("Failed to locate object (mode %s, class %s): ", mode(object), class(object)[1])
}
