library(environments)

message("*** Primitive functions cannot be pruned")

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
