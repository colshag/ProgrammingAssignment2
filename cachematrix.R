## The goal of these two functions is to compute the inverse of a matrix 
## and cache that result so that if called again the cache can be used to
## save computing time on large loops rather than computing the inverse
## on every iteration.

## makeCacheMatrix creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = setinverse)
}


##  CacheSolve computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above. If the matrix has not changed, then the 
##  fucntion will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
