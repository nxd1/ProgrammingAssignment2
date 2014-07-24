## Contains functions to create a "super-matrix" object and cache its inverse.
## Assumes the matrix supplied is invertible.

## Function to create a special matrix that returns a list of functions to:
## get matrix
## get the inverse of the matrix
## set matrix values
## set the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {

  xinv <- NULL
  
  # methods to get matrix, get inverse and get original
  get <- function() x
  getinv <- function() xinv
  
  # method to re/set new values for x after initial creation and cache it,
  # resetting the previously cached inverse
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  
  # method to set a new inverse provided
  setinv <- function(new.inv) xinv <<- new.inv
  
  # return list of setters/getters
  list(get = get,
      getinv = getinv,
      set = set,
      setinv = setinv)
}


## Function to determine whether a new inverse needs to be calculated for
## a "special" matrix.

cacheSolve <- function(x, ...) {
  
  # fetch cached inverse
  inv <- x$getinv()
  
  # calculate new inverse if not cached, then set it in the cache
  if (is.null(inv)) {
    message("calculating new inverse")
    inv <- solve(x$get(), ...)
    x$setinv(inv)
  }
  
  # return a matrix that is the inverse of 'x'
  inv 
}
