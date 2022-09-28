a <- 1  ## <== f() only sees this 'a'

f <- function() {
  b <- 2

  ## Get the environments scanned for 'a'. This shows that the current
  ## environment (= environment()) is first searched, then the parent
  ## environments of 'f' (= parent_envs(f)) are searched. This is the
  ## reason why 'a' (a == 1) in the top environment is used. Note that,
  ## if we call f() from g(), the environment from where f() is called,
  ## which holds another 'a' (a == 42), is *not* involved. Thus, we get
  ## the same result regardless from where f() is called.
  envirs <- search_path(until = globalenv())
  utils::str(envirs)
  
  b * a
}

g <- function() {
  a <- 42  ## <== this 'a' is never seen by f()
  f()
}

f()

g()
