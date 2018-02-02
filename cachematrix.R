## makeCacheMatrix creates an object with content = matrix, and methods
## for pitting and getting - and setting and getting the inverse.

## makeCacheMatrix creates an object with a matrix x and methods set, get,
## setInverse (for creating the inverse of x using function 'solve', and
## getInverse (for returning the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve uses the method getInverse on an object created by 'makeCacheMatrix'
## to return the cached inverse, or to calculate it, using 'solve'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i

}
