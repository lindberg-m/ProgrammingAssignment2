## Functions for performing and caching cpu/memory-consuming calculations
#
#  Create a cachable matrix with the makeCacheMatrix() function,
#  and solve said matrix with cacheSolve(). Once it's solved,
#  cacheSolve will return the cached result and not perform the calculation.
#  
#  These are wrappers around the more general functions makeCache()
#  and cacheApply(), which are used to apply a function and store its result
#  together with the function used.
#

# Create a cache:
# the cache (m) is accessed and set through functions returned in the list
# of makeCache().
#
# Variable descriptions:
#   x : The original dataset
#   m : The cached result
#   f : The function used to compute the result

makeCache <- function(x) {
    m <- NULL
    f <- NULL
    set <- function(y) {
        x <<- y       # Reinitialize the original dataset
        m <<- NULL    # Empty the cache
        f <<- NULL    # Empty the cached function 
    }
    get <- function() x                      # Get the original dataset
    setCached <- function(x) m <<- x         # Set the cached result
    setCachedFunc <- function(fun) f <<- fun # Set the function for computing m
    getCached <- function() m                # Get the cached result "m"
    getCachedFunc <- function() f            # Get the functoin used for 
                                             #  computing the cached result m
    list(get = get,
         set= set,
         getCached = getCached,
         getCachedFunc = getCachedFunc,
         setCached = setCached,
         setCachedFunc = setCachedFunc)
}

# Apply the function "f" to the dataset "x", cache result and 
# applied function
cacheApply <- function(x, f, ...) {
    res  <- x$getCached()       # Get the cahced data
    func <- x$getCachedFunc()   # Get the function used for computing "res"
    if (!is.null(res) && identical(f, func)) {
        # If there is a cached result, ensure that it is the same function
        # that was used in order to compute the result that is passed by
        # the user
        message("getting cached data")
        return(res)
    }
    
    # Computation and caching:
    data <- x$get()      # Get the original data
    res <- f(data, ...)  # Compute the result from applying f to the dataset
    x$setCached(res)     # Save result in cache
    x$setCachedFunc(f)   # Save what function was used to compute "res"
    res                  # Return result
}

makeCacheMatrix <- function(x = matrix()) makeCache(x)
cacheSolve      <- function(x, ...) cacheApply(x, solve, ...)
