library(environments)
library(stats)

message("*** Setup")

## WORKAROUND: R CMD check adds '.Last.value' when running tests
prune_check_env <- function(envir = parent.frame()) {
  if (exists(".Last.value", envir = envir, inherits = FALSE)) {
    rm(".Last.value", envir = envir)
  }
}

message("*** Locate stats::rnorm()")
res <- find_object_by_value(stats::rnorm)
str(res)
stopifnot(
  is.list(res),
  res$name == "rnorm",
  identical(res$envir, as.environment("package:stats"))
)


message("*** Locate local copy of stats::rnorm()")
my_rnorm <- stats::rnorm
prune_check_env()
res <- find_object_by_value(my_rnorm)
str(res)
stopifnot(
  is.list(res),
  res$name == "my_rnorm",
  identical(res$envir, environment())
)


message("*** Locate stats::rnorm() from parent environment")
my_rnorm <- stats::rnorm
prune_check_env()
res <- find_object_by_value(my_rnorm, from = parent.env(environment()))
str(res)
stopifnot(
  is.list(res),
  res$name == "rnorm",
  identical(res$envir, as.environment("package:stats"))
)


message("*** Locate stats::rnorm() despite local copy")
my_rnorm <- stats::rnorm
prune_check_env()
res <- find_object_by_value(my_rnorm, which = "last")
str(res)
stopifnot(
  is.list(res),
  res$name == "rnorm",
  identical(res$envir, as.environment("package:stats"))
)


message("*** Locate local-environment copy of stats::rnorm()")
local({
  my_rnorm <- stats::rnorm
  res <- find_object_by_value(my_rnorm)
  str(res)
  stopifnot(
    is.list(res),
    res$name == "my_rnorm",
    identical(res$envir, environment())
  )
})


message("*** Locate from within function")

my_find <- function(object, which = "first") {
  find_object_by_value(object, which = which)
}

my_fcn <- local({
  g <- stats::rnorm  ## this is never found
  function(f = NULL) {
    if (is.null(f)) {
      f <- g  ## this is found first
      my_find(f)
    } else {
      my_find(f, which = "last") # skip argument 'f'
    }
  }
})

res <- my_fcn()
str(res)

res <- my_fcn(my_rnorm)
str(res)
