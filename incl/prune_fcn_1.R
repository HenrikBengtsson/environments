## ------------------------------------------------------------------------
## Example how to avoid huge local objects being part of a local function,
## which might be costly if the function is serialized, e.g. exported
## to a parallel workers
## ------------------------------------------------------------------------

## Call a function with the option to replace the function
## environment with a smaller temporary environment
my_do_call <- function(fcn, args = list(), envir = parent.frame(), prune = FALSE) {
  fcn_name <- as.character(substitute(fcn))
  if (prune) {
    fcn <- prune_fcn(fcn, search = find_object_by_value(fcn, from = envir, first = FALSE)$envir)
    
    ## Important: We must drop attribute 'prune_undo' before
    ## exporting object, otherwise it will carry the pruned
    ## environment as cargo
    fcn_undo <- attr(fcn, "prune_undo")
    attr(fcn, "prune_undo") <- NULL
    
    on.exit(fcn_undo())
  }
  
  do.call(fcn, args = args, envir = envir)
}

## Report on the 'fcn' size before and after pruning
trace(
  my_do_call,
  at = 3L, tracer = quote(fcn_size <- size_of(fcn)),
  exit = quote({
    if (prune) {
      message(sprintf("Size of '%s': %s bytes (%s bytes when pruned)",
                      fcn_name, fcn_size, size_of(fcn)))
    } else {
      message(sprintf("Size of '%s': %s bytes",
                      fcn_name, fcn_size))
    }
  }),
  print = FALSE
)

my_fcn <- function(g = NULL, prune = FALSE) {
  cargo <- rnorm(1e6)
  
  n <- 2

  if (is.null(g)) {
    g <- local({
      pi <- 3.14
      function() n * pi
    })
  }

  my_do_call(g, prune = prune)
}


## Non-pruned function local to a function carries also large 'cargo' object
my_fcn()

## Pruned function local to a function without large 'cargo' object
my_fcn(prune = TRUE)


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

my_fcn(g)
my_fcn(g, prune = TRUE)
