library(environments)

find_object(name = "pi")
find_object(name = "pi", mode = "character")   ## non-existing
find_object(name = "rnorm", mode = "function")

f <- local({
  a <- 42
  pi <- 3.14
  function() pi * a
})
env <- find_object(name = "a", from = f)
utils::ls.str(env)

f <- local({
  a <- 42
  local({
    pi <- 3.14
    function() pi * a
  })
})
env_a <- find_object(name = "a", from = f)
utils::ls.str(env_a)
env_pi <- find_object(name = "pi", from = f)
utils::ls.str(env_pi)
stopifnot(
  identical(environment(f), env_pi),
  identical(parent.env(env_pi), env_a)
)

