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
    fcn <- prune_fcn(fcn, search = locate_object(fcn, from = envir)$envir)
    
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



## g2 <- local({
##   cargo <- rnorm(1e6)
##   n <- 2
##   local({
##     pi <- 3.14
##     function() n * pi
##   })
## })
## 
## ## Non-pruned global function does not carry large 'cargo' object,
## ## because it's located in the global environment, which is never
## ## serialized/exported
## my_fcn(g2)
## 
## ## Pruning a global function makes no difference
## my_fcn(g2, prune = TRUE)
## 
## ## Proof that g2() is only temporarily pruned and undone automatically
## my_fcn(g2)
