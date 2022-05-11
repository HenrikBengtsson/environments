## Example how to do the above temporarily inside a function
my_fcn <- function(globals = NULL) {
  n <- 1
  g <- local({
    pi <- 3.14
    function() n * pi
  })
  if (length(globals) > 0) {
    new <- as.environment(globals)
    g_env <- environment(g)
    old <- replace_env(g_env, search = environment(), replace = new)
    on.exit(replace_env(g_env, search = new, replace = old))
  }
  g()
}

my_fcn()
my_fcn(globals = list(n = 2))
