# Version (development version)

## Significant Changes

 * Argument `until` for `parent_envs()` now defaults to `globalenv()`
   rather than `emptyenv()`.

 * Merge `locate_object()` into a new `find_object()` function.

 * Now `environment_name()` returns `"package:base"` for `baseenv()`
   to distinguish it from `"base"` returned for
   `getNamespace("base")`.  `baseenv()` is the same as
   `pos.to.env(grep("package:base", search()))`.
   
## Documentation

 * Add a package vignette.


# Version 0.3.0 (2022-09-27)

## New Features

* `prune_fcn()` will now skip already pruned functions.

* Add `do_call()`.

## Bug Fixes

* `environment_name()` would return more than one string if
  the environment had attributes.
  

# Version 0.2.0 (2022-05-19)

## New Features

* Add support for `prune_fcn()` with `depth > 1`.

* Now `locate_object()` ignores `base::.Last.value`.

* Now `prune_fcn()` ignores primitive functions.

* Moved **cli** to `Suggests:`.


## Bug Fixes

* `prune_fcn()` gave an error with `globals = list()`.


# Version 0.1.0 (2022-05-16)

## New Features

* Created package with functions `environment_name()`, `find_object()`,
  `locate_object()`, `get_globals()`, `parent_env()`, `parent_envs()`,
  `top_env()`, `replace_env()`, `prune_fcn()`, `search_path()`,
  `size_of()`, `draw_search()`, and `boxx_search()`.


