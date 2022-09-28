#' Remove unused objects from a function's parent environments
#'
#' @inheritParams replace_env
#'
#' @param fcn A \code{\link[base:function]{function}}.
#'
#' @param globals A named list of variables to be part of the injected
#' environment.
#'
#' @param depth Depth of recursive pruning.
#' If `depth = 0L`, no globals are pruned.
#' If `depth = 1L`, "first-generation" functions among the globals are pruned.
#' If `depth = 2L`, functions among the globals of the first-generation
#' functions are pruned.
#' And, so on.
#'
#' @return A pruned version of `fcn`, with `prune_undo` attribute holding
#' an "undo" function. _WARNING: Make sure to copy this attribute and then
#' remove it before exporting the function to an external process._
#'
#' @details
#' An already pruned function will skipped, by returning the pruned version.
#' This works by setting attribute `pruned` of the injected environment
#' (the new `environment(fcn)`) to TRUE, and checking for such a flag in
#' each call to `prune_fcn()`.
#'
#' @example incl/prune_fcn_1.R
#' 
#' @export
prune_fcn <- function(fcn, search = locate_object(fcn, from = parent.frame(), first = FALSE)$envir, globals = get_globals(fcn), depth = 0L) {
  stopifnot(is.function(fcn))

  ## Already pruned?
  fcn_env <- environment(fcn)
  if (isTRUE(attr(fcn_env, "pruned"))) return(fcn)

  ## Nothing to do?
  if (is.primitive(fcn)) return(fcn)
  
  if (!is.list(search)) {
    search <- list(search)
    names(search) <- environment_name(search[[1]])
  }
  for (env in search) stopifnot(inherits(env, "environment"))
  if (is.null(globals)) globals <- list()
  stopifnot(is.list(globals), length(names(globals)) == length(globals))
  stopifnot(length(depth) == 1L, is.numeric(depth), !is.na(depth), depth >= 0L)

  undo_data <- attr(fcn, "prune_undo_data")
  if (is.null(undo_data)) undo_data <- list()
  
  if (length(globals) >= 0L && depth >= 1L) {
#    if (depth > 1L) {
#      stop(sprintf("Only depth = 0L and depth = 1L is implemented: %d", depth))
#    }
    for (name in names(globals)) {
      global <- globals[[name]]
      if (is.function(global)) {
        global <- prune_fcn(global, search = environment(fcn), depth = depth - 1L)
        prune_undo <- attr(global, "prune_undo")
        attr(global, "undo_data") <- NULL
        undos <- environment(prune_undo)[["undo_data"]]
        stopifnot(is.list(undos))
        if (length(undos) > 0) undo_data <- c(undo_data, undos)
      }
    }
  }
  
  new <- as.environment(globals)
  attr(new, "pruned") <- TRUE
  old <- replace_env(fcn_env, search = search, replace = new)

  ## Special case: Nothing replaced?
  if (identical(old, fcn_env)) {
    parent.env(new) <- parent.env(fcn_env)
    environment(fcn) <- new
    undo <- list(
      fcn = fcn,
      new = NULL,
      old = old
    )
  } else {
    undo <- list(
      fcn = fcn,
      new = new,
      old = old
    )
  }  
  undo_data <- c(undo_data, list(undo))

  prune_undo <- local({
    undo_data <- list()
    function() {
      fcn <- NULL
      for (undo in undo_data) {
        fcn <- undo$fcn
        stopifnot(is.function(fcn))
        if (is.null(undo$new)) {
          environment(fcn) <- undo$old
        } else {
          replace_env(fcn, search = undo$new, replace = undo$old)
        }
      }
      fcn
    }
  })
  environment(prune_undo)[["undo_data"]] <- undo_data
  attr(fcn, "prune_undo") <- prune_undo
  
  fcn
}
