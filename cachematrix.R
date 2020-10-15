## This file contains functions that cache the inverse of a matrix so that the
## inverse does not need to be computed repeatedly.  In these functions, we
## assume the matrix supplied is invertible.

## makeCacheMatrix -- creates a special "matrix" object that can cache its
## inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function()
    x
  set_inverse <- function(new_inverse)
    inverse <<- new_inverse
  get_inverse <- function()
    inverse
  list(
    set = set,
    get = get,
    set_inverse = set_inverse,
    get_inverse = get_inverse
  )
  
  
}


## cacheSolve -- computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$get_inverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  inverse
}
