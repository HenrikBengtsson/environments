library(environments)

## Temporary file used in this test

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
  identical(environment(f)$pi, 3.14),
  identical(parent.env(environment(f))$a, 2),
  length(parent.env(environment(f))$cargo) == 1e6
)
tf <- tempfile(fileext = ".rds")
saveRDS(f, file = tf)
truth <- f()
print(truth)


message("*** Exporting manually modified function in global environment with globals")
## Wrap f() and 'a' up in a local environment
new <- new.env()
new$a <- a
parent.env(environment(f)) <- new
stopifnot(
  identical(environment(f)$pi, 3.14),
  identical(parent.env(environment(f))$a, 2),
  is.null(parent.env(environment(f))$cargo)
)
tf2 <- tempfile(fileext = ".rds")
saveRDS(f, file = tf2)


message("*** Exporting replace_env() modified function in global environment with globals")
new <- as.environment(list(a = a))
replace_env(environment(f), search = locate_object(f)$envir, replace = new)
stopifnot(
  identical(environment(f)$pi, 3.14),
  identical(parent.env(environment(f))$a, 2),
  is.null(parent.env(environment(f))$cargo)
)
tf3 <- tempfile(fileext = ".rds")
saveRDS(f, file = tf3)

rm(list = c("cargo", "a", "f", "new"))


message("*** Verify exported function without globals")
f <- readRDS(tf)
stopifnot(
  identical(environment(f)$pi, 3.14),
  is.null(parent.env(environment(f))$a),
  is.null(parent.env(environment(f))$cargo)
)
res <- tryCatch({ f() }, error = identity)
print(res)
stopifnot(inherits(res, "error"))
rm(list = c("f"))


message("*** Verify manually modified function")

f <- readRDS(tf2)
stopifnot(
  identical(environment(f)$pi, 3.14),
  identical(parent.env(environment(f))$a, 2),
  is.null(parent.env(environment(f))$cargo)
)
res <- f()
print(res)
stopifnot(identical(res, truth))
rm(list = c("f"))


message("*** Verify replace_env() modified function")

f <- readRDS(tf3)
stopifnot(
  identical(environment(f)$pi, 3.14),
  identical(parent.env(environment(f))$a, 2),
  is.null(parent.env(environment(f))$cargo)
)
res <- f()
print(res)
stopifnot(identical(res, truth))
rm(list = c("f"))


message("*** Cleanup")
invisible(file.remove(c(tf, tf2, tf3)))

