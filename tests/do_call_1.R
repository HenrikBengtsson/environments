library(environments)

## Setup
## Report on the 'fcn' size before and after pruning
trace(
  do_call,
  at = 3L, tracer = quote(fcn_size_0 <<- size_of(fcn)),
  exit = quote({
    fcn_size_1 <<- size_of(fcn)
    if (prune) {
      message(sprintf("Size of '%s': %s bytes (%s bytes when pruned)",
                      fcn_name, fcn_size_0, fcn_size_1))
    } else {
      message(sprintf("Size of '%s': %s bytes", fcn_name, fcn_size_1))
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


message("*** my_fcn()")
## Non-pruned function local to a function carries also large 'cargo' object
truth <- my_fcn()
message("truth: ", truth)
rel_size <- fcn_size_1 / fcn_size_0
message("Relative size: ", rel_size)
stopifnot(
  fcn_size_0 > 8e6,
  fcn_size_1 > 8e6,
  abs(rel_size - 1) < 0.01
)

## Pruned function local to a function without large 'cargo' object
res <- my_fcn(prune = TRUE)
message("Result: ", res)
stopifnot(identical(res, truth))
rel_size <- fcn_size_1 / fcn_size_0
message("Relative size: ", rel_size)
stopifnot(
  fcn_size_0 > 8e6,
  fcn_size_1 < 100e3,
  abs(rel_size - 0) < 0.01
)


message("*** my_fcn(g)")
cargo <- rnorm(1e6)
g <- local({
  n <- 2
  local({
    pi <- 3.14
    function() n * pi
  })
})

truth <- my_fcn(g)
message("truth: ", truth)
rel_size <- fcn_size_1 / fcn_size_0
message("Relative size: ", rel_size)
stopifnot(
  fcn_size_1 < 100e3
)

res <- my_fcn(g, prune = TRUE)
message("Result: ", res)
stopifnot(identical(res, truth))
rel_size <- fcn_size_1 / fcn_size_0
message("Relative size: ", rel_size)
stopifnot(
  fcn_size_1 < 100e3
)


message("*** my_fcn(g2)")
local({
  cargo <- rnorm(1e6)
  g2 <- local({
    n <- 2
    local({
      pi <- 3.14
      function() n * pi
    })
  })
  
  truth <- my_fcn(g2)
  message("truth: ", truth)
  rel_size <- fcn_size_1 / fcn_size_0
  message("Relative size: ", rel_size)
  stopifnot(abs(rel_size - 1) < 0.01)

  ## FIXME: Gives:
  ## Error in replace_env(fcn_env, search = search, replace = new) : 
  ## Cannot replace environment. None of the environments specified ...
#  res <- my_fcn(g2, prune = TRUE)
#  message("Result: ", res)
#  stopifnot(identical(res, truth))
#  rel_size <- fcn_size_1 / fcn_size_0
#  message("Relative size: ", rel_size)
#  stopifnot(abs(rel_size - 1) < 0.01)
})


message("*** my_fcn(g3)")
## WARNING: Large objects inside local environments of
##          the function will not the pruned!
g3 <- local({
  cargo <- rnorm(1e6)
  n <- 2
  local({
    pi <- 3.14
    function() n * pi
  })
})

truth <- my_fcn(g3)
message("truth: ", truth)
rel_size <- fcn_size_1 / fcn_size_0
message("Relative size: ", rel_size)
stopifnot(
  fcn_size_0 > 8e6,
  fcn_size_1 > 8e6,
  abs(rel_size - 1) < 0.01
)

res <- my_fcn(g3, prune = TRUE)
message("Result: ", res)
stopifnot(identical(res, truth))
rel_size <- fcn_size_1 / fcn_size_0
message("Relative size: ", rel_size)
stopifnot(
  fcn_size_0 > 8e6,
  fcn_size_1 > 8e6,
  abs(rel_size - 1) < 0.01
)


## Cleanup
untrace(do_call)
