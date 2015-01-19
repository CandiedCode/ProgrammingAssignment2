makeCacheMatrix <- function(x = matrix()) {
    # This function creates a special "matrix" object that can cache its inverse.
    #
    # Args:
    #   x: matrix
    #
    # Returns:
    #   list with functions
    #    $set - sets matrix to x
    #    $get - gets matrix x
    #    $setInverse - sets inverse matrix
    #    $getInverse - returns inverse matrix
    #
    inverse <- NULL
    set <- function(y) {
        x <<-y
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <-function(solve) {
        inverse <<- solve
    }
    getInverse <- function() {
        inverse
    }
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
    # This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
    # If the inverse has already been calculated (and the matrix has not changed), then the
    # cachesolve should retrieve the inverse from the cache.
    # Args:
    #   x: matrix
    #
    # Returns:
    #   inverse matrix of x
    #
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data,...)
    x$setInverse(inverse)
    inverse
}