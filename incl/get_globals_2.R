my_fcn <- function(prune = FALSE) {
  huge <- rnorm(1e6)
  
  n <- 2
  
  g <- local({
    pi <- 3.14
    function() n * pi
  })

  globals <- globals::globalsOf(g, envir = environment(g), mustExist = FALSE)
  str(globals)
  globals <- globals::cleanup(globals)
  str(globals)
  fcn_globals <- get_globals(g)
  str(fcn_globals)
}


my_fcn()
