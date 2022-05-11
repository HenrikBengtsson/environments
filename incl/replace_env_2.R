## Example how to avoid huge local objects being part of a local function,
## which might be costly if the function is serialized, e.g. exported
## to a parallel workers
size_of <- function(object) {
  con <- rawConnection(raw(), open = "w")
  on.exit(close(con))
  serialize(object, connection = con)
  length(rawConnectionValue(con))
}

my_fcn <- function(prune = FALSE) {
  huge <- rnorm(1e6)
  
  n <- 2
  g <- local({
    pi <- 3.14
    function() n * pi
  })

  if (prune) {
    ## Emulate globals::globalsOf(g)
    globals <- list(n = n)
    new <- as.environment(globals)
    g_env <- environment(g)
    old <- replace_env(g_env, search = environment(), replace = new)
    on.exit(replace_env(g_env, search = new, replace = old))
  }

  message(sprintf("Size of g(): %s bytes", size_of(g)))
  
  g()
}

my_fcn()
my_fcn(prune = TRUE)
my_fcn()
