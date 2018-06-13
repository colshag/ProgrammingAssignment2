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
             getinverse = getinverse)
}


##  CacheSolve computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above. If the matrix has not changed, then the 
##  function will retrieve the inverse from the cache.

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

## This assignment is not clear on how to test it, I found
## a post from the forum on how to test it. I also do not
## understand how we grade each other when we don't know if
## we did this correctly.. :)

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
solve(m1)
###returns this
#[,1] [,2]
#[1,]    6    8
#[2,]    2    4
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
###returns this
#[,1] [,2]
#[1,]    6    8
#[2,]    2    4
