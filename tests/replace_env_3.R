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
  globals <- get_globals(f)
  for (name in names(globals)) {
    global <- globals[[name]]
    if (is.function(global)) {
      ## Prune
      if (FALSE) {
        gg <- get_globals(global)
        new <- as.environment(gg)
        old <- replace_env(global, search = environment(), replace = new)
        if (identical(old, environment(global))) {
          parent.env(new) <- parent.env(environment(global))
          environment(global) <- new
        }
      } else {
        global <- prune_fcn(global)
        fcn_undo <- attr(global, "prune_undo")
        attr(global, "prune_undo") <- NULL
        on.exit(fcn_undo(), add = TRUE)
      }

      stopifnot(is.null(parent_env(global, n = 0)$cargo))
      globals[[name]] <- global
    }
  }

  stopifnot(
    identical(parent_env(globals$g, n = 0)$a, 1),
    identical(parent_env(globals$h, n = 0)$a, 2),
      is.null(parent_env(globals$g, n = 1)$cargo),
      is.null(parent_env(globals$h, n = 1)$cargo)
  )

  if (FALSE) {
    new <- as.environment(globals)
    old <- replace_env(f, search = environment(), replace = new)
    if (identical(old, environment(f))) {
      parent.env(new) <- parent.env(environment(f))
      environment(f) <- new
    }
  } else {
    f <- prune_fcn(f, search = environment(f), globals = globals)
    fcn_undo <- attr(f, "prune_undo")
    attr(f, "prune_undo") <- NULL
    on.exit(fcn_undo(), add = TRUE)
  }

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
