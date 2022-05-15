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
    fcn_globals <- get_globals(fcn)
    new <- as.environment(fcn_globals)
    old <- replace_env(fcn, search = fcn_where$envir, replace = new)
    on.exit(replace_env(fcn, search = new, replace = old))
  }
  
  message(sprintf("Size of '%s': %s bytes",
          as.character(substitute(fcn)), size_of(fcn)))
 
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
