## The following functions are used in conjunction for 
## returning the inverse of a matrix either from cache
## if run previously or by way calculation using solve.

## Creates a cacheable matrix for use by 'cacheSolve'. 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Calculates the inverse of a matrix created by 'makeCacheMatrix'
## and caches it otherwise if a cached version exists return it.
cacheSolve <- function(x, ...) {
        m <- x$get()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$getinverse()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}