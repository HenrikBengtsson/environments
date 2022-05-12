locate_object(rnorm)

my_sum <- rnorm
locate_object(my_sum)

locate_object(my_sum, from = parent.env(environment()))

my_sum <- local(sum)
locate_object(my_sum)


my_find <- function(object, envir = parent.frame()) {
  locate_object(object, from = envir)
}
my_fcn <- function() {
  my_sum <- sum
  my_find(my_sum)
}
my_fcn()

