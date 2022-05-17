library(environments)

## Related to https://github.com/HenrikBengtsson/future/issues/608

message("*** Setup")

tf <- tempfile(fileext = ".rds")

message("*** Name-clashes of variables in local function environment")
truth <- NULL

f2 <- local({
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

  g_envs <<- parent_envs(g)
  h_envs <<- parent_envs(h)
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
        is.null(parent_env(parent_env(f, n = 0)$g, n = 1)$cargo),
        is.null(parent_env(parent_env(f, n = 0)$h, n = 1)$cargo)
  )

  g_envs_pruned <<- parent_envs(g)
  h_envs_pruned <<- parent_envs(h)
  f_envs_pruned <<- parent_envs(f)
  stopifnot(!identical(f_envs_pruned, f_envs))
  
  saveRDS(f, file = tf)
  message(sprintf("Size of exported 'f': %s bytes", file.size(tf)))

  ## Undo pruning
  message("undoing ...")
  print(prune_undo)
  str(environment(prune_undo)$undo_data)
  f_org <- prune_undo()
  ## disable registered undoing on exit
  prune_undo <- function() NULL

  ## Assert undoing of f():s environments
  str(list(
    f_org = names(f_envs),
    f_pruned = names(f_envs_pruned),
    f_undone = names(parent_envs(f_org))
  ))
  stopifnot(identical(parent_envs(f_org), f_envs))
  
  ## Assert undoing of g():s environments
  str(list(
    g_org = names(g_envs),
    g_pruned = names(g_envs_pruned),
    f_undone = names(parent_envs(g))
  ))
  stopifnot(identical(parent_envs(g), g_envs))
  
  ## Assert undoing of h():s environments
  str(list(
    h_org = names(h_envs),
    h_pruned = names(h_envs_pruned),
    f_undone = names(parent_envs(h))
  ))
  stopifnot(identical(parent_envs(h), h_envs))

  message("undoing ... done")

  f_org
})

## Assert that nothing have leaked to the calling environment
stopifnot(!any(c("f", "g", "h", "cargo") %in% names(environment())))

## Assert undoing of f()/f2():s environments
str(list(
  f_org = names(f_envs),
  f_pruned = names(f_envs_pruned),
  f_undone = names(parent_envs(f2))
))
stopifnot(identical(parent_envs(f2), f_envs))

## Assert undoing of g():s environments
str(list(
  g_org = names(g_envs),
  g_pruned = names(g_envs_pruned),
  f_undone = names(parent_envs(parent_env(f2, n = 0)$g))
))
stopifnot(identical(parent_envs(parent_env(f2, n = 0)$g), g_envs))

## Assert undoing of h():s environments
str(list(
  h_org = names(h_envs),
  h_pruned = names(h_envs_pruned),
  f_undone = names(parent_envs(parent_env(f2, n = 0)$h))
))
stopifnot(identical(parent_envs(parent_env(f2, n = 0)$h), h_envs))


## Sanity checks (verify that undoing of pruning happens)
stopifnot(
  is.function(parent_env(f2, n = 0)$g),
  is.function(parent_env(f2, n = 0)$h),
       length(parent_env(f2, n = 0)$cargo) == 1e6,
       length(parent_env(f2, n = 0)$cargo) == 1e6,
    identical(parent_env(parent_env(f2, n = 0)$g, n = 0)$a, 1),
    identical(parent_env(parent_env(f2, n = 0)$h, n = 0)$a, 2)
)

g <- parent_env(f2, n = 0)$g
h <- parent_env(f2, n = 0)$h
stopifnot(
  is.function(g),
  is.function(h),
  length(parent_env(g, n = 1)$cargo) == 1e6,
  length(parent_env(h, n = 1)$cargo) == 1e6
)
rm(list = c("g", "h"))


## Assert expected behavior
f <- readRDS(tf)
res <- f()
print(res)
stopifnot(identical(res, truth))
rm(list = c("f"))


message("*** Cleanup")
invisible(file.remove(c(tf)))
