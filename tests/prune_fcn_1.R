library(environments)

message("*** Setup")

tf <- tempfile(fileext = ".rds")
tf2 <- tempfile(fileext = ".rds")


message("*** Exporting function part of a local environment")

local({
  cargo <- rnorm(1e6)
  a <- 2
  f <- local({
    pi <- 3.14
    function() {
      n <- 4
      a * pi / n
    }
  })
  stopifnot(
    identical(parent_env(f, n = 0)$pi, 3.14),
    identical(parent_env(f, n = 1)$a, 2),
       length(parent_env(f, n = 1)$cargo) == 1e6
  )
  saveRDS(f, file = tf)
  truth <<- f()
  print(truth)

  prune_fcn(f)
  stopifnot(
    identical(parent_env(f, n = 0)$pi, 3.14),
    identical(parent_env(f, n = 1)$a, 2),
      is.null(parent_env(f, n = 1)$cargo)
  )
  saveRDS(f, file = tf2)
})  


message("*** Verify exported functions")

## Since we everything is in a local environment when f() is serialized,
## all of the local environment is brought along
f <- readRDS(tf)
stopifnot(
  identical(parent_env(f, n = 0)$pi, 3.14),
  identical(parent_env(f, n = 1)$a, 2),
     length(parent_env(f, n = 1)$cargo) == 1e6
)
res <- f()
print(res)
stopifnot(identical(res, truth))
rm(list = c("f"))


## However, the serialized, modified versions behave as expected
f <- readRDS(tf2)
stopifnot(
  identical(parent_env(f, n = 0)$pi, 3.14),
  identical(parent_env(f, n = 1)$a, 2),
    is.null(parent_env(f, n = 1)$cargo)
)
res <- f()
print(res)
stopifnot(identical(res, truth))
rm(list = c("f"))


message("*** Cleanup")
invisible(file.remove(c(tf, tf2)))
