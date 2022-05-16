library(environments)

## Related to https://github.com/HenrikBengtsson/future/issues/608

message("*** Setup")

tf <- tempfile(fileext = ".rds")

message("*** Name-clashes of variables in local function environment")
truth <- NULL

f <- local({
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

  ## Sanity checks
  stopifnot(
    identical(parent_env(g, n = 0)$a, 1),
    identical(parent_env(h, n = 0)$a, 2),
       length(parent_env(g, n = 1)$cargo) == 1e6,
       length(parent_env(h, n = 1)$cargo) == 1e6
  )

  f_envs <<- parent_envs(f)

  ## Prune
  f <- prune_fcn(f, search = environment(f), depth = 1L)
  prune_undo <- attr(f, "prune_undo")
  attr(f, "prune_undo") <- NULL
  on.exit(prune_undo(), add = TRUE)

  ## Sanity checks
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

  f_envs_pruned <<- parent_envs(f)
  stopifnot(!identical(f_envs_pruned, f_envs))
  
  saveRDS(f, file = tf)
  message(sprintf("Size of exported 'f': %s bytes", file.size(tf)))

  ## Undo pruning
  f_org <- prune_undo()
  ## disable registered undoing on exit
  prune_undo <- function() NULL

  ## FIXME: Undoing of pruning doesn't work /HB 2022-05-16
  if (FALSE) {
    str(f_org)
    str(names(parent_envs(f_org)))
    stopifnot(identical(parent_envs(f_org), f_envs))
  }
  
  f_org
})

str(list(
  f_org = names(f_envs),
  f_pruned = names(f_envs_pruned),
  f_undone = names(parent_envs(f))
))


## Sanity checks (verify that undoing of pruning happens)
globals <- get_globals(f)
stopifnot(
    identical(parent_env(globals$g, n = 0)$a, 1),
    identical(parent_env(globals$h, n = 0)$a, 2),
       length(parent_env(globals$g, n = 1)$cargo) == 1e6,
       length(parent_env(globals$h, n = 1)$cargo) == 1e6,
  is.function(parent_env(f, n = 0)$g),
  is.function(parent_env(f, n = 0)$h),
    identical(parent_env(parent_env(f, n = 0)$g, n = 0)$a, 1),
    identical(parent_env(parent_env(f, n = 0)$h, n = 0)$a, 2)
)

## FIXME: Undoing of pruning doesn't work /HB 2022-05-16
if (FALSE) {
  stopifnot(
    length(parent_env(parent_env(f, n = 0)$g, n = 0)$cargo) == 1e6,
    length(parent_env(parent_env(f, n = 0)$h, n = 0)$cargo) == 1e6
  )
}



## Assert expected behavior
f <- readRDS(tf)
res <- f()
print(res)
stopifnot(identical(res, truth))
rm(list = c("f"))


message("*** Cleanup")
invisible(file.remove(c(tf)))
