## ------------------------------------------------------------------------
## Example how to avoid huge local objects being part of a local function,
## which might be costly if the function is serialized, e.g. exported
## to a parallel workers
## ------------------------------------------------------------------------

## Report on the 'fcn' size before and after pruning
trace(
  do_call,
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

  do_call(g, prune = prune)
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

untrace(do_call)
