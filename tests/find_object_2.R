library(environments)

find_object(value = rnorm)

my_sum <- rnorm
find_object(value = my_sum)

find_object(value = my_sum, from = parent_env())

my_sum <- local(sum)
find_object(value = my_sum)


my_find <- function(object, envir = parent.frame()) {
  find_object(value = object, from = envir)
}

my_fcn <- local({
  g <- sum  ## this is never found
  function(f = NULL) {
    if (is.null(f)) {
      f <- g  ## this is found first
      my_find(f)
    } else {
      my_find(f, envir = parent.frame()) # skip argument 'f'
    }
  }
})
my_fcn()
my_fcn(my_sum)

topenv()
