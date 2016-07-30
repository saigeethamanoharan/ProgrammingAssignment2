
## Assignment: Caching the Inverse of a Matrix
## Assumption :  The input matrix is always inversable

## This function returns a special vector that holds a list of functions to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(matrixInverse) i <<- matrixInverse
    getInverse <- function()i
    list(set = set, get = get,
         setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function returns the inverse of a matrix from cache is available 
## else computes the inverse and stores the result in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matInverse <- x$getInverse()
    if(!is.null(matInverse)){
        message("returning cached data")
        return(matInverse)
    }
    matData <- x$get()
    matInverse <- solve(matData)
    x$setInverse(matInverse)
    matInverse
}


