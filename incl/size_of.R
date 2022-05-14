## Size of integer vector 1, 2, ..., 1000
size_of(seq(from = 1L, to = 1000L, by = 1L))

## Size of ALTREP representation of integer vector 1, 2, ..., 1000
size_of(1:1000)


## Functions created locally carry the weight of other objects
## in the same environment as the function
make_fcn <- function(n) {
  cargo <- sample.int(n)
  a <- 42
  function() a
}

## Size of teeny 'cargo' + 'a' + a little bit more
size_of(make_fcn(0))

## Size of huge 'cargo' + 'a' + a little bit more
size_of(make_fcn(1e6))



