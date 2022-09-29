#' Locate the original name and location of an object
#'
#' @inheritParams find_object
#'
#' @param object The R object whose location should be identified.
#'
#' @param which If `"first"` or `"last"`, then the first or the last
#' occurance of `object` among the parent frames is identified and returned.
#' If `"all"`, then all occurances are returned.
#'
#' @return
#' If `which = "first"` or `which = "last"`, then a named list with
#' elements `name` and `envir`, where `name` is the name of `object`
#' as it is named in environment `envir`, i.e.
#' `identical(envir[[name]], object)`.
#' If `which = "all"`, then a list of (name, environment) lists are
#' returned; one for each matching occurence.
#' If the object could not be located when searching from environment
#' `from`, then NULL is returned.
#'
#' @example incl/find_object_by_value_1.R
#'
#' @export
find_object_by_value <- function(object, from = parent.frame(), until = emptyenv(), which = c("first", "last", "all")) {
  if (inherits(from, "environment")) {
    envir <- from
  } else {
    envir <- environment(from)
    if (!inherits(envir, "environment")) {
      stop(sprintf("Argument 'from' does not specify an environment or an object with an environment: %s", mode(from)))
    }
  }

  if (!is.list(until)) until <- list(until)
  for (env in until) stopifnot(inherits(env, "environment"))

  ## Make sure there's always an empty environment at the end
  until <- c(until, list(emptyenv()))

  in_until <- function(envir) {
    for (env in until) {
      if (identical(envir, env)) return(TRUE)
    }
    FALSE
  }
  
  which <- match.arg(which)
  
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

  res <- list()
  while (!in_until(envir)) {
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
          res_name <- list(name = name, envir = envir)
          if (which == "first") return(res_name)
          res <- c(res, list(res_name))
        }
      }
    }
    envir <- parent.env(envir)
  }

  if (length(res) == 0L) return(NULL)
  if (which == "last") res <- res[[length(res)]]
  res
}

