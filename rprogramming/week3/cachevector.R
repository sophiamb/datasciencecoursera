# example


# The key concept to understand in makeVector() is that it builds a set of functions and returns the functions 
# within a list to the parent environment. That is,
#     myVector <- makeVector(1:15)
# results in an object, myVector, that contains four functions: set(), get(), setmean(), and getmean(). It also
# includes the two data objects, x and m.

# Due to lexical scoping, myVector contains a complete copy of the environment for makeVector(), including any
# objects that are defined within makeVector() at design time (i.e., when it was coded).

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}