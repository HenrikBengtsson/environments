## ------------------------------------------------------------------------
## Example how to avoid huge local objects being part of a local function,
## which might be costly if the function is serialized, e.g. exported
## to a parallel workers
## ------------------------------------------------------------------------

## Call a function with the option to replace the function
## environment with a smaller temporary environment
do_call <- function(fcn, args = list(), envir = parent.frame(),
                    prune = FALSE) {
  if (prune) {
    fcn_where <- locate_object(fcn, from = envir)
    fcn_parents <- parent_envs(fcn, until = fcn_where$envir)
    fcn_globals <- get_globals(fcn)
    if (getOption("debug", FALSE)) {
      utils::str(list(
          fcn_where = fcn_where,
        fcn_parents = fcn_parents,
        fcn_globals = fcn_globals
      ))
      stopifnot(any(vapply(fcn_parents, FUN.VALUE = FALSE, FUN = identical, fcn_where$envir)))
    }
    new <- as.environment(fcn_globals)
    message("Now environment holding globals:")
    print(new)
    search <- list(fcn_env = environment(fcn), "fcn_where$envir" = fcn_where$envir)
    old <- replace_env(fcn, search = search, replace = new)
    on.exit(replace_env(fcn, search = new, replace = old))
  }
  
  message(sprintf("Size of '%s': %s bytes",
          as.character(substitute(fcn)), size_of(fcn)))

    fcn_where <- locate_object(fcn, from = envir)
    fcn_parents <- parent_envs(fcn, until = list(fcn_where$envir, globalenv()))
    fcn_globals <- get_globals(fcn)
    if (getOption("debug", FALSE)) {
      utils::str(list(
          fcn_where = fcn_where,
        fcn_parents = fcn_parents,
        fcn_globals = fcn_globals
      ))
#      stopifnot(any(vapply(fcn_parents, FUN.VALUE = FALSE, FUN = identical, fcn_where$envir)))
      print(ls.str(environment(fcn)))
      for (name in names(fcn_parents)[1:2]) {
        message(sprintf("%s:", name))
        print(ls.str(fcn_parents[[name]]))
      }
    }

  print(ls.str(environment(fcn)))

  do.call(fcn, args = args, envir = envir)
}

my_fcn <- function(prune = FALSE) {
  cargo <- rnorm(1e6)
  
  n <- 2
  g <- local({
    pi <- 3.14
    function() n * pi
  })
  
  do_call(g, prune = prune)
}

my_fcn()
my_fcn(prune = TRUE)


n <- 2  
g <- local({
  pi <- 3.14
  function() n * pi
})

my_fcn <- function(prune = FALSE) {
  cargo <- rnorm(1e6)
  do_call(g, prune = prune)
}

my_fcn()
my_fcn(prune = TRUE)


cargo <- rnorm(1e6)
n <- 2  
g <- local({
  pi <- 3.14
  function() n * pi
})

my_fcn <- function(prune = FALSE) {
  do_call(g, prune = prune)
}

my_fcn()
my_fcn(prune = TRUE)

rm(list = c("cargo", "n"))


## WARNING: Large objects inside local environments of
##          the function will not the pruned!
g <- local({
  cargo <- rnorm(1e6)
  n <- 2  
  local({
    pi <- 3.14
    function() n * pi
  })
})

my_fcn <- function(prune = FALSE) {
  do_call(g, prune = prune)
}

my_fcn()
my_fcn(prune = TRUE)
