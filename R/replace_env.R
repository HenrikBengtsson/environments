#' Replace one of the parent environments with another
#'
#' @param envir An \code{\link[base:environment]{environment}}.
#'
#' @param search A \code{\link[base:environment]{environment}},
#' among the parents of `envir`, to be replaced.
#' It is possible to specify a list of alternative environments.
#'
#' @param replace A \code{\link[base:environment]{environment}}.
#'
#' @param update_parent If TRUE, or 1L, the parent environment of
#' `replace` is set to the parent environment of the replaced
#' "search" environment. If FALSE, or 0L, it is not updated.
#' If a positive integer greater than one, then that parent
#' generation is updated, e.g. `update_parent = 2L` will update
#' the parent environment of the _parent_ of `replace`.
#'
#' @return Invisibly, the replaced environment.
#'
#' @section Replace single environment or a sequence of environments:
#'
#' Consider below function `f()` where `pi` is part of `environment(f)`,
#' which is a local environment, and `a` is a global variable part of
#' `parent.env(environment(f))`, which we _denote_ as `environment^2(f)`:
#'
#' ```r
#' cargo <- rnorm(1e6)
#' a <- 2
#' f <- local({
#'   pi <- 3.14
#'   function() {
#'     n <- 4
#'     a * pi / n
#'   }
#' })
#' ```
#'
#' We can visualize this as:
#'
#' ```
#' +----------------------+
#' | environment^2(f):    |
#' | cargo = { 1e6 }      |
#' | a = 2                |
#' | f                    |
#' +----------------------+
#'            ^
#'            |
#' +----------------------+
#' | environment(f):      |
#' | pi = 3.14            |
#' +----------------------+
#'            ^
#'            |
#' +======================+
#' | f():                 | (frame at runtime)
#' | n = 4                |
#' +======================+
#' ```
#'
#' In order to evaluate `f()`, variables `a` and `pi`, which are global
#' ("free") variables defined outside and not at runtime in the call
#' frame, like `n` is.  To clarify further what the difference is:
#' we cannot query `n` from `f`, but we can query both `pi` and `a` as
#' `environment(f)$pi` and `parent.env(environment(f))$a`.
#' Similarly, we can also do `parent.env(environment(f))$cargo`, but
#' it is a variable useless for evalating `f()`.
#'
#' When we serialize `f` (e.g. export it to a parallel worker), the body
#' and the formals of the function is included, as well as all
#' the environments of `f` up to where `f` itself lives, e.g.
#' `environment(f)` and `parent.env(environment(f))` in our example.
#' However, if the environment where `f` lives is the global environment
#' (= `globalenv()`), then it is _not_ part of the serialization output.
#' Imagine we save `f` to file, restart R, and load it back, e.g.
#'
#' ```r
#' saveRDS(f, "f.rds")
#' quit(save = "no")
#' f <- readRDS(f)
#' ```
#'
#' In this case, we will lose `a` and `cargo`, which is good and bad.
#' It's bad, because we need to bring `a` back, in order to evaluate `f()`,
#' e.g.
#'
#' ```r
#' f()
#' #> Error in f() : object 'a' not found
#' ```
#'
#' It's good, because we don't have to pay the price of serializing the
#' large `cargo` object.  Continuing, our unserialized `f()` looks like:
#'
#' ```
#' +----------------------+
#' | environment^2(f):    | (= globalenv())
#' | f                    |
#' +----------------------+
#'            ^
#'            |
#' +----------------------+
#' | environment(f):      |
#' | pi = 3.14            |
#' +----------------------+
#'            ^
#'            |
#' +======================+
#' | f():                 | (frame at runtime)
#' | n = 4                |
#' +======================+
#' ```
#'
#' One way to revive `a` is to inject a new grandparent environment that
#' holds a copy of `a`, e.g.
#'
#' ```r
#' new <- new.env(parent = parent.env(environment(f)))
#' new$a <- 2
#' parent.env(environment(f)) <- new
#' ```
#'
#' ```
#' +----------------------+
#' | environment^3(f):    | (= globalenv())
#' | f                    |
#' +----------------------+
#'            ^
#'            |
#' +----------------------+
#' | environment^2(f):    | (injected environment)
#' | a = 2                |
#' +----------------------+
#'            ^
#'            |
#' +----------------------+
#' | environment(f):      |
#' | pi = 3.14            |
#' +----------------------+
#'            ^
#'            |
#' +======================+
#' | f():                 | (frame at runtime)
#' | n = 4                |
#' +======================+
#' ```
#'
#' and we can evaluate `f()` again;
#'
#' ```r
#' f()
#' #> 1.57
#' ```
#'
#' We can of course built up the above version of `f()` _before_ serializing,
#' e.g before we saved to file above. Then it is ready to use when
#' unserialized, e.g. read back from file.
#' An alternative way to achive this is to use the `replace_env()` function;
#'
#' ```r
#' new <- as.environment(list(a = a))
#' replace_env(environment(f), search = locate_object(f)$envir, replace = new)
#' ```
#'
#' If we save this to file, restart R, and load it back in, we'll see that
#' we have a fully functional version of `f`, e.g. `f()` gives 1.57.
#' 
#' @example incl/replace_env.R
#' @example incl/replace_env_2.R
#'
#' @export
replace_env <- function(envir, search, replace, update_parent = TRUE) {
  stopifnot(inherits(envir, "environment"))
  if (!is.list(search)) {
    search <- list(search)
    names(search) <- environment_name(search[[1]])
  }
  for (env in search) stopifnot(inherits(env, "environment"))
  stopifnot(inherits(replace, "environment"))
  stopifnot(length(update_parent) == 1L, !is.na(update_parent))
  if (is.logical(update_parent)) update_parent <- as.integer(update_parent)
  stopifnot(is.numeric(update_parent), !is.na(update_parent),
            update_parent >= 0L)

  envirs <- parent_envs(envir, until = search)

  ## Assert that a match was found
  found <- FALSE
  for (env in search) {
    for (penv in envirs) {
      if (identical(penv, env)) {
        found <- TRUE
        break
      }
    }
    if (found) break
  }
  if (!found) {
    stop(sprintf("Cannot replace environment. None of the environments specified in 'search' (%s) are among the parent environments of 'envir': %s", paste(sQuote(names(search)), collapse = ", "), paste(sQuote(names(envirs)), collapse = ", ")))
  }
  
  ## Nothing to do?
  n <- length(envirs)
  last <- envirs[[n]]
  if (n == 1L) return(last)

  child <- envirs[[n - 1L]]
  parent.env(child) <- replace

  ## Update parent environment of 'replace'?
  if (update_parent > 0L) {
    ## (a) Identify new parent environment
    if (identical(last, emptyenv())) {
      ## Special case: replace the empty environment
      last_parent <- emptyenv()
    } else {
      last_parent <- parent.env(last)
    }
    
    ## (b) Update parent environment of generation 'update_parent'
    count <- update_parent - 1L
    while (!identical(replace, emptyenv()) && count > 0L) {
      replace <- parent.env(replace)
      count <- count - 1L
    }
    if (identical(replace, emptyenv())) {
      stop(sprintf("Cannot replace parent generation %d of 'replace', because it either doesn't exist or is the empty environment", update_parent))
    }
    parent.env(replace) <- last_parent
  }
  
  invisible(last)
}
