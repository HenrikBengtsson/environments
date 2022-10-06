#' Find the environment where an object exists
#'
#' @inheritParams parent_envs
#'
#' @param value The R object whose location should be identified.
#'
#' @param name The name of the object to locate.
#'
#' @param mode The \code{\link[base:mode]{mode}} of the object to locate.
#'
#' @param from An \code{\link[base:environment]{environment}}, or an object
#' with an environment (e.g. a \code{\link[base:function]{function}} and a
#' \code{\link[base:tilde]{formula}}), to start search from.
#'
#' @param which If `"first"` or `"last"`, then the first or the last
#' occurrence of `object` among the parent frames is identified and returned.
#' If `"all"`, then all occurrences are returned.
#'
#' @return
#' A named list with elements `name` and `envir`, where `name` is the
#' name of the located object in environment `envir`.
#' If no object could be located, then NULL is returned.
#' If more than one matching object could be located, then a list of
#' all matching (name, environment) lists is returned if `which = "all"`.
#' If `which = "first"`, then the first match is returned,
#' and if `which = "last"`, then the last match is returned.
#'
#' @section Environments searched:
#' A matching object is looked for in environment `from`. If it is found
#' there, then `from` is returned.  If not found there, the parent
#' environment of `from` is searched, and so on, until an environment in
#' `until`, or the "empty" environment
#' (\code{\link[base:emptyenv]{emptyenv()}}) is reached. In such cases,
#' no matching object could be found and NULL is returned.
#'
#' @section Find an object by its value:
#' `find_object()` with arguments `value` locates an object of any name
#' with value `value` in one of the environments searched.
#'
#' @section Find an object by its name and mode:
#' `find_object()` with arguments `name` and `mode` locates an object
#' with name `name` and mode `mode` in one of the environments searched.
#' This is how [base::exists()], [base::get()], and [base::assign()] locate
#' an object based on its name and mode.
#' For example, `exists(name) == !is.null(find_object(name = name))`.
#" Similarly, `object <- get(name)` is the same as:
#'
#' ```r
#' envir <- find_object(name = name)
#' if (is.null(envir)) stop(sprintf("Object %s not found", sQuote(name)))
#' object <- get(name, envir = envir, inherits = FALSE)
#' ```
#'
#' @example incl/find_object_1.R
#' @example incl/find_object_2.R
#'
#' @export
find_object <- function(value = NULL, name = NULL, mode = "any", from = parent.frame(), until = emptyenv(), which = c("first", "last", "all")) {
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

  res <- list()
  if (!is.null(name)) {
    if (!is.null(value)) mode <- mode(value)
    
    while (!in_until(envir)) {
      res_name <- NULL
      if (exists(name, mode = mode, envir = envir, inherits = FALSE)) {
        res_name <- list(name = name, envir = envir)

        ## Match also value?
        if (!is.null(value)) {
          res_value <- get(name, mode = mode, envir = envir, inherits = FALSE)
          ## No match?
          if (!identical(res_value, value)) res_name <- NULL
        }
      }
      
      ## Found a match?
      if (!is.null(res_name)) {
        if (which == "first") return(res_name)
        res <- c(res, list(res_name))
      }
      
      envir <- parent.env(envir)
    }
  } else if (!is.null(value)) {
    ## WORKAROUND: R CMD check will add mockup 'F' and 'T' objects
    ## to catch misuse of 'F' instead of 'FALSE' and 'T' and 'TRUE'
    ## in examples. Here we detect if example() is running under
    ## 'R CMD check'.  If it does, we'll skip searching that
    ## environment.
    skip <- NULL
    if ("CheckExEnv" %in% search()) {
      skip <- as.environment("CheckExEnv")
    }

    mode <- mode(value) ## scan for objects of this mode

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
        ## ... consider only those with the same mode as 'value' ...
        if (exists(name, mode = mode, envir = envir, inherits = FALSE)) {
          tmp <- get(name, mode = mode, envir = envir, inherits = FALSE)
          ## ... are the identical?
          if (identical(tmp, value, ignore.environment = FALSE, ignore.bytecode = FALSE, ignore.srcref = FALSE)) {
            res_name <- list(name = name, envir = envir)
            if (which == "first") return(res_name)
            res <- c(res, list(res_name))
          }
        }
      }
      envir <- parent.env(envir)
    }
  } else {
    stop("Either argument 'name' or 'value' must be specified and non-NULL")
  }

  if (length(res) == 0L) return(NULL)
  if (which == "last") res <- res[[length(res)]]
  res
}
