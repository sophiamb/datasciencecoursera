## Coursera RProgramming Week 3 Assignment


## Make a matrix which supports caching of the inverse value
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #store inverse as i
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  
  getinverse <- function() i
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Return the inverse of a matrix, from cache if possible
cacheSolve <- function(x, ...) {
  # Check for cached inverse and return cached value if it exists...
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  
  # If there's no cached value, return calculated inverse 
  data <- x$get()
  message("Calculating inverse")
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
