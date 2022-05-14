a <- 1

f <- function() {
  a <- 42
  g()
}

g <- function() {
  b <- 2

  ## Get the environments scanned for 'a'. This shows that the current
  ## environment (= environment()) is first searched, then the parent
  ## environments of 'g' (= parent_envs(environment(g))) are searched.
  ## This is the reason why 'a' (a == 1) in the top environment is used.
  ## Note that the environment from where g() was called, which holds
  ## another 'a' (a == 42), is *not* involved.
  envirs <- search_path(until = globalenv())
  utils::str(envirs)
  
  b * a
}
