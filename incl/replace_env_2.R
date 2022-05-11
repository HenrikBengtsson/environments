## ------------------------------------------------------------------------
## Example how to avoid huge local objects being part of a local function,
## which might be costly if the function is serialized, e.g. exported
## to a parallel workers
## ------------------------------------------------------------------------

## True size of an object
size_of <- function(object) {
  con <- rawConnection(raw(), open = "w")
  on.exit(close(con))
  serialize(object, connection = con)
  length(rawConnectionValue(con))
}

## Call a function with the option to replace the function
## environment with a smaller temporary environment
do_call <- function(fcn, args = list(), envir = parent.frame(), globals = NULL) {
  if (length(globals) > 0) {
    fcn_env <- environment(fcn)
    new <- as.environment(globals)
    old <- replace_env(fcn_env, search = envir, replace = new)
    on.exit(replace_env(fcn_env, search = new, replace = old))
  }
  
  message(sprintf("Size of '%s': %s bytes",
          as.character(substitute(fcn)), size_of(fcn)))
  do.call(fcn, args = args, envir = envir)
}

my_fcn <- function(prune = FALSE) {
  huge <- rnorm(1e6)
  
  n <- 2
  g <- local({
    pi <- 3.14
    function() n * pi
  })

  globals <- if (prune) {
    list(n = n)  ## Emulate globals::globalsOf(g)
  } else {
    NULL
  }
  
  do_call(g, globals = globals)
}

my_fcn()
my_fcn(prune = TRUE)
my_fcn()
