## We want to cache the inverse of a matrix so that we don't have to calculate
## the inverse every time. We do this by making a special matrix object that
## can cache its inverse, and a function that can use this object to find the
## inverse of a matrix.

## This function makes a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL
    set <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) xInv <<- inverse
    getInverse <- function() xInv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function calculates the inverse of a matrix, using the cached inverse if
## possible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xInv <- x$getInverse()
    if(!is.null(xInv)) {
        message("getting cached data")
        return(xInv)
    }
    data <- x$get()
    xInv <- solve(data, ...)
    x$setInverse(xInv)
    xInv
}
