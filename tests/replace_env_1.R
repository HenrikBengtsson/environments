library(environments)

message("*** Setup")

tf <- tempfile(fileext = ".rds")
tf2 <- tempfile(fileext = ".rds")
tf3 <- tempfile(fileext = ".rds")


message("*** Exporting function in global environment (without globals)")
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
truth <- f()
print(truth)


message("*** Exporting manually modified function in global environment with globals")
## Wrap f() and 'a' up in a local environment
new <- new.env()
new$a <- a
parent.env(environment(f)) <- new
stopifnot(
  identical(parent_env(f, n = 0)$pi, 3.14),
  identical(parent_env(f, n = 1)$a, 2),
    is.null(parent_env(f, n = 1)$cargo)
)
saveRDS(f, file = tf2)


message("*** Exporting replace_env() modified function in global environment with globals")
new <- as.environment(list(a = a))
replace_env(environment(f), search = find_object(value = f)$envir, replace = new)
stopifnot(
  identical(parent_env(f, n = 0)$pi, 3.14),
  identical(parent_env(f, n = 1)$a, 2),
    is.null(parent_env(f, n = 1)$cargo)
)
suppressWarnings(saveRDS(f, file = tf3))  ## produces a warning; expected

rm(list = c("cargo", "a", "f", "new"))


message("*** Verify exported function without globals")
f <- readRDS(tf)
stopifnot(
  identical(parent_env(f, n = 0)$pi, 3.14),
    is.null(parent_env(f, n = 1)$a),
    is.null(parent_env(f, n = 1)$cargo)
)
res <- tryCatch({ f() }, error = identity)
print(res)
stopifnot(inherits(res, "error"))
rm(list = c("f"))


message("*** Verify manually modified function")

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


message("*** Verify replace_env() modified function")

f <- readRDS(tf3)
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
invisible(file.remove(c(tf, tf2, tf3)))

