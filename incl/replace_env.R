a <- 42
f <- local(function() a)

f_envs <- parent_envs(f, until = environment(), extra = 1L)
names(f_envs)
y <- f()
y

new <- as.environment(list(a = 13, pi = 3.14))
old <- replace_env(f, search = environment(), replace = new)
old

f2_envs <- parent_envs(f, until = list(environment(), parent.env(environment())))
names(f2_envs)

## Note that f() will now see a = 13 in the replaced environment
## rather than a = 42 in the calling environment
z <- f()
z

## Undo changes
old2 <- replace_env(f, search = new, replace = old)
stopifnot(identical(old2, new))

f3_envs <- parent_envs(f, until = environment(), extra = 1L)
stopifnot(identical(f3_envs, f_envs))

## f() will now see a = 42 again
z <- f()
z

