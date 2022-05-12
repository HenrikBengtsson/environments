library(environments)

ls_str_envs <- function(envirs) {
  for (kk in seq_along(envirs)) {
    cat(sprintf("%d. %s:\n", kk, names(envirs)[[kk]]))
    env <- envirs[[kk]]
    stopifnot(inherits(env, "environment"))
    print(utils::ls.str(env))
  }
}

caller <- function(fcn, envir = parent.frame()) {
  message("caller() ...")
  on.exit(message("caller() ... done"))
  envs <- parent_envs(environment(fcn), until = globalenv())
  print(envs)
  last <- envs[[length(envs)]]
  last2 <- envs[[length(envs) - 1L]]
#  envs <- envs[-length(envs)]
#  ls_str_envs(envs)
  if (identical(fcn, last2$g)) {
    penv <- new.env(parent = parent.env(last))
    penv$a <- 13
    print(last2)
    environment(last2) <- penv
    print(last2)
    fcn <- last2$g
    environment(last2) <- penv
  }
  do.call(fcn, args = list(), envir = envir)
}

a <- 42
f <- local({
  pi <- 3.14
  function() a
})

y <- caller(f)
print(y)
stopifnot(y == 42)

my_caller <- function() {
  message("my_caller() ...")
  on.exit(message("my_caller() ... done"))
  pi <- 3.14
  g <- function() a
  caller(g)
}

y <- local(my_caller())
print(y)
stopifnot(y == 42)
