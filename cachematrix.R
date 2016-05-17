## Functions for calculating and caching the inverse of a matrix.
#  The two functions makeCacheMatrix() and cacheSolve() are supposed to
#  be used together.
#
#  Create a cachable matrix with the makeCacheMatrix() function,
#  and solve said matrix with cacheSolve

## Make a list containing both the matrix and 
#  a way to store its inverse, identical to the example in the README.md
#  but with a matrix inverse cached, instead of a mean
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get        <- function() x
    setInverse <- function(inv) m <<- inv
    getInverse <- function() m
    
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)
}


## A function that returnes the cached inverse matrix of 'x'. If no
#  inversed matrix can be found, calculate it anew. Inspired
#  by the example in README.md
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
