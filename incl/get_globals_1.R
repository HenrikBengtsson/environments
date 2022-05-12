## Here 'a' is a global variable
a <- 42
f <- function() pi * a
globals <- get_globals(f)
utils::ls.str(globals)

## Here 'a' is not a global variable, because it is part of
## the environment of 'f', which is a local environment
## that comes with function 'f'
f <- local({
  a <- 42
  function() pi * a
})
globals <- get_globals(f)
utils::ls.str(globals)


## Same here; 'a' is not a global variable
f <- local({
  a <- 42
  local({
    function() pi * a
  })
})
globals <- get_globals(f)
utils::ls.str(globals)

