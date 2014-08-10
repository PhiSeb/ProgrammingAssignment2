## This script define provide 2 function useful to cache 
## the inverse of matrix.

## This function creates a list that 'wraps' a matrix.
## Arguments:
##    x   a matrix
## Returned value:
##        a list that contain 4 function:
##         - get    returns the matrix.
##         - set    sets the matrix and set the cached inverse to null.
##         - setInv sets the cached inverse of the matrix.
##         - getInv returns the cached inverse of the matrix, can be null.
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
##    x   A list containing the input matrix. The list must be constructed
##        with the function makeCacheMatrix.
## Return value:
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
