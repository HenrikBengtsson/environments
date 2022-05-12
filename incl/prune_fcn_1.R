## ------------------------------------------------------------------------
## Example how to avoid huge local objects being part of a local function,
## which might be costly if the function is serialized, e.g. exported
## to a parallel workers
## ------------------------------------------------------------------------

## Call a function with the option to replace the function
## environment with a smaller temporary environment
do_call <- function(fcn, args = list(), envir = parent.frame(),
                    prune = FALSE) {
  fcn_name <- as.character(substitute(fcn))
  if (prune) {
    fcn <- prune_fcn(fcn, search = parent.frame())

    ## Important: We must drop attribute 'prune_undo' before
    ## exporting object, otherwise it will carry the pruned
    ## environment as cargo
    fcn_undo <- attr(fcn, "prune_undo")
    attr(fcn, "prune_undo") <- NULL
    
    on.exit(fcn_undo())
  }
  
  message(sprintf("Size of '%s': %s bytes", fcn_name, size_of(fcn)))
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
my_fcn()
