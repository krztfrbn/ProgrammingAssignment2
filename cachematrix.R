## Put comments here that give an overall description of what your
## functions do

## Basic functions for cache matrix creation
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ## set inverse
  setinverse <- function(mean) m <<- solve
  ## get inverse
  getinverse <- function() m
  ## list available functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## return cached data
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    ## cache already set - return
    message("getting cached data")
    return(m)
  }
  ## cache not set yet - populate with inverse of matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  ## return cache contents
  m
}
