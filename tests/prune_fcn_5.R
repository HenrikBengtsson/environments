library(environments)

message("*** Primitive functions cannot be pruned")

f <- base::sum
stopifnot(is.primitive(f))

g <- prune_fcn(f)
stopifnot(identical(g, f))
