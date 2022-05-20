library(environments)

message("*** Primitive functions are skipped because they cannot be pruned")

f <- base::sum
stopifnot(is.primitive(f))

g <- prune_fcn(f)
stopifnot(identical(g, f))


message("*** Prune stats::rnorm")
set.seed(42L)
truth <- stats::rnorm(10)
my_rnorm <- prune_fcn(stats::rnorm)
set.seed(42L)
y <- my_rnorm(10)
stopifnot(identical(y, truth))

prune_undo <- attr(my_rnorm, "prune_undo")
prune_undo()


message("*** Prune stats::rnorm (2)")
my_rnorm <- prune_fcn(stats::rnorm, search = environment(stats::rnorm))
res <- tryCatch(y <- my_rnorm(10), error = identity)
stopifnot(inherits(res, "error"))
prune_undo <- attr(my_rnorm, "prune_undo")
prune_undo()


message("*** Already pruned functions are skipped")
a <- 42
f <- function() a

## Prune
g <- prune_fcn(f)
stopifnot(!identical(g, f))

## Assert 'pruned' flag is set
g_env <- environment(g)
stopifnot(isTRUE(attr(g_env, "pruned")))

## Assert 'pruned' flag is not set on f()
f_env <- environment(f)
pruned <- attr(f_env, "pruned")
stopifnot(!isTRUE(attr(f_env, "pruned")))

## Assert a pruned function is not pruned again
h <- prune_fcn(g)
stopifnot(identical(h, g))
h_env <- environment(h)
stopifnot(identical(h_env, g_env))

