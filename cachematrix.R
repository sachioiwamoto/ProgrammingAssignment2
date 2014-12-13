## Matrix inversion is usually a costly computation. The two functions below
## will cache the inverse of a matrix to compute it repeatedly.

## makeCacheMatrix function creates a special "matrix" object that can cache
## its inverse. The code is based on the sampe function,"makeVector".
## I simply changed "numeric" and "mean" to "matrix" and "solve" respectively.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
                setsolve = setsolve,
                getsolve = getsolve)
}

## cacheSolve function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.
## Here again the code is based on "cachemean.R" sample.
## I simply changed "mean" to "solve".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
