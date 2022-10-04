find_object_by_name <- function(name, mode = "any", from = parent.frame(), until = emptyenv(), which = c("first", "last", "all")) {
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
  while (!in_until(envir)) {
    if (exists(name, mode = mode, envir = envir, inherits = FALSE)) {
      res_name <- list(name = name, envir = envir)
      if (which == "first") return(res_name)
      res <- c(res, list(res_name))
    }
    envir <- parent.env(envir)
  }
  
  if (length(res) == 0L) return(NULL)
  if (which == "last") res <- res[[length(res)]]
  res
}
