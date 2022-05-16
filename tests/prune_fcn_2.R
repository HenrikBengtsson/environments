library(environments)

## Related to https://github.com/HenrikBengtsson/future/issues/608

message("*** Setup")

tf <- tempfile(fileext = ".rds")

message("*** Two local functions with same-name local variables")

f <- function() {
  cargo <- rnorm(1e6)
  
  g <- local({
    a <- 1
    function() a
  })

  h <- local({
    a <- 2
    function() a
  })

  stopifnot(
    identical(parent_env(g, n = 0)$a, 1),
    identical(parent_env(h, n = 0)$a, 2),
       length(parent_env(g, n = 1)$cargo) == 1e6,
       length(parent_env(h, n = 1)$cargo) == 1e6
  )

  g() * h()
}

saveRDS(f, file = tf)
message(sprintf("Size of exported 'f': %s bytes", file.size(tf)))

truth <<- f()
print(truth)
rm(list = c("f"))

## Assert expected behavior
f <- readRDS(tf)
res <- f()
print(res)
stopifnot(identical(res, truth))
rm(list = c("f"))



message("*** Ditto but pruned internal functions")

f <- function() {
  cargo <- rnorm(1e6)
  
  g <- local({
    a <- 1
    function() a
  })

  h <- local({
    a <- 2
    function() a
  })

  prune_fcn(g)
  prune_fcn(h)
  stopifnot(
    identical(parent_env(g, n = 0)$a, 1),
    identical(parent_env(h, n = 0)$a, 2),
      is.null(parent_env(g, n = 1)$cargo),
      is.null(parent_env(h, n = 1)$cargo)
  )

  g() * h()
}

saveRDS(f, file = tf)
message(sprintf("Size of exported 'f': %s bytes", file.size(tf)))

rm(list = c("f"))

## Assert expected behavior
f <- readRDS(tf)
res <- f()
print(res)
stopifnot(identical(res, truth))
rm(list = c("f"))


message("*** Similar with internal functions g() and h() pruned, but not f() itself")

local({
  cargo <- rnorm(1e6)
  
  f <- function() {
    g <- local({
      a <- 1
      function() a
    })
  
    h <- local({
      a <- 2
      function() a
    })
  
    prune_fcn(g)
    prune_fcn(h)
    stopifnot(
      identical(parent_env(g, n = 0)$a, 1),
      identical(parent_env(h, n = 0)$a, 2),
        is.null(parent_env(g, n = 1)$cargo),
        is.null(parent_env(h, n = 1)$cargo)
    )
  
    g() * h()
  }
  
  saveRDS(f, file = tf)
  message(sprintf("Size of exported 'f': %s bytes", file.size(tf)))
})


## Assert expected behavior
f <- readRDS(tf)
res <- f()
print(res)
stopifnot(identical(res, truth))
rm(list = c("f"))



message("*** Function with \"cargo\" without automatic pruning")
local({
  cargo <- rnorm(1e6)
  
  f <- local({
    g <- local({
      a <- 1
      function() a
    })
  
    h <- local({
      a <- 2
      function() a
    })
  
    function() g() * h()
  })

  res <- f()
  stopifnot(identical(res, truth))

  saveRDS(f, file = tf)
  message(sprintf("Size of exported 'f': %s bytes", file.size(tf)))
})

## Assert expected behavior
f <- readRDS(tf)
res <- f()
print(res)
stopifnot(identical(res, truth))
rm(list = c("f"))


message("*** Ditto with automatic pruning")
local({
  cargo <- rnorm(1e6)
  
  f <- local({
    g <- local({
      a <- 1
      function() a
    })
  
    h <- local({
      a <- 2
      function() a
    })
  
    function() g() * h()
  })

  res <- f()
  stopifnot(identical(res, truth))

  prune_fcn(f)

  saveRDS(f, file = tf)
  message(sprintf("Size of exported 'f': %s bytes", file.size(tf)))
})

## Assert expected behavior
f <- readRDS(tf)
res <- f()
print(res)
stopifnot(identical(res, truth))
rm(list = c("f"))


message("*** Cleanup")
invisible(file.remove(c(tf)))
