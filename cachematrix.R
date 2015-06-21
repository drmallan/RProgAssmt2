## This function stores four functions:
## set() : changes the matrix
## get() : returns the matrix unchanged
## setinv() : changes the value of the inverse (don't calculate, only store the value)
## getinv() : returns the value unchanged 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverx) inv <<- inverx
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  ## return a list of functions
}

## input of this function is the object where makeCacheMatrix is stored
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
    ## verify the value inv exists and is not NULL. If it exist, return a message and the original value
  }
  ## or calculate the inverse and return the calculated value
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}