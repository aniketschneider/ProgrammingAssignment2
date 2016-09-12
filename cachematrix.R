## This pair of functions allow caching of matrix inverse computations.
## cacheSolve operates on the containers produced by makeCacheMatrix and
## computes the inverse of the matrix stored in the container. If the
## container already contains a cached inverse, the cached inverse is returned
## instead.

## Creates a container for a matrix that allows a cached inverse to be stored
## alongside the matrix itself.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(newMatrix) {
    x <<- newMatrix
    inv <<- NULL
  }
  get <- function() x
  setCachedInv <- function(newInv) {
    inv <<- newInv
  }
  getCachedInv <- function() inv
  list(set = set, get = get,
       setCachedInv = setCachedInv,
       getCachedInv = getCachedInv)
}


## Computes the inverse of a matrix. If the inverse of the matrix has already
## been computed, the cached inverse is returned. This function only operates
## on matrix containers created by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  cachedInv <- x$getCachedInv()
  if (!is.null(cachedInv)) {
    message("Using cached inverse")
    cachedInv
  } else {
    originalMatrix <- x$get()
    cachedInv <- solve(originalMatrix, ...)
    x$setCachedInv(cachedInv)
    cachedInv
  }
}
