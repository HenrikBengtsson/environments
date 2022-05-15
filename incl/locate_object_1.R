locate_object(rnorm)

my_sum <- rnorm
locate_object(my_sum)

locate_object(my_sum, from = parent.env(environment()))

my_sum <- local(sum)
locate_object(my_sum)


my_find <- function(object, envir = parent.frame()) {
  locate_object(object, from = envir)
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
