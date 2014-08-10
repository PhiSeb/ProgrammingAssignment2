## This script define provide 2 function usefull to cache 
## the inverse of matrix.

## This function creates a list that 'wrap' a matrix.
## Arguments:
##    x   a matirx
## Retrun value:
##        a list that contain 4 function:
##         - get    Return the matirix.
##         - set    Set the matirix and set the cached inverse to null.
##         - setInv Set the cached inverse of the matrix.
##         - getInv Return the cached inverse of the matrix, can be null.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(invers) inv <<- invers
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## This function computes the inverse of a matrix and cache the result
## Arguments:
##    x   A list contening the input matrix. The list must be construct
##        with the function makeCacheMatrix.
## Retrun value:
##        The inverse of the given matirix.
cacheSolve <- function(x) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    ## Return the cached inverse.
    return(inv)
  }
  ## Compute and cache the inverse of 'x'.
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}
