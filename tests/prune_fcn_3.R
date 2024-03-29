library(environments)

## Related to https://github.com/HenrikBengtsson/future/issues/608

message("*** Setup")

tf <- tempfile(fileext = ".rds")

message("*** Name-clashes of variables in local function environment")
truth <- NULL

local({
  cargo <- rnorm(1e6)
  
  g <- local({
    a <- 1
    function() a
  })

  h <- local({
    a <- 2
    function() a
  })

  f <- function() g() * h()

  truth <<- f()

  stopifnot(
    identical(parent_env(g, n = 0)$a, 1),
    identical(parent_env(h, n = 0)$a, 2),
       length(parent_env(g, n = 1)$cargo) == 1e6,
       length(parent_env(h, n = 1)$cargo) == 1e6
  )

  ## Get globals and prune any functions among them
  prune_undos <- list()
  globals <- get_globals(f)
  for (name in names(globals)) {
    global <- globals[[name]]
    if (is.function(global)) {
      ## Prune
      global <- prune_fcn(global)
      prune_undo <- attr(global, "prune_undo")
      attr(global, "prune_undo") <- NULL
      prune_undos[[name]] <- prune_undo
    }
  }

  f <- prune_fcn(f, search = environment(f), globals = globals)
  prune_undo <- attr(f, "prune_undo")
  attr(f, "prune_undo") <- NULL
  prune_undos <- c(prune_undos, prune_undo)

  on.exit({ for (prune_undo in prune_undos) prune_undo() }, add = TRUE)

  globals <- get_globals(f)
  stopifnot(
      identical(parent_env(globals$g, n = 0)$a, 1),
      identical(parent_env(globals$h, n = 0)$a, 2),
        is.null(parent_env(globals$g, n = 1)$cargo),
        is.null(parent_env(globals$h, n = 1)$cargo),
    is.function(parent_env(f, n = 0)$g),
    is.function(parent_env(f, n = 0)$h),
      identical(parent_env(parent_env(f, n = 0)$g, n = 0)$a, 1),
      identical(parent_env(parent_env(f, n = 0)$h, n = 0)$a, 2),
        is.null(parent_env(parent_env(f, n = 0)$g, n = 0)$cargo),
        is.null(parent_env(parent_env(f, n = 0)$h, n = 0)$cargo)
  )

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
