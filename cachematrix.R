## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix returns a cached matrix
# (actually a list with 4 functions; set, get, setinv and getinv).
# This cachematrix is an input for cachesolve.
# Example usage:
# source("$nameRfile.R")
# matrA <- matrix(c(1,2,3,4),nrow=2,ncol=2)
# matrACache <-makeCacheMatrix(matrA)
# cachesolve(matrACache) # calculates the inverse
# cachesolve(matrACache) # second call now returns cached matrix
# matrACache <-makeCacheMatrix(matrA) ## makeCacheMatrix call resets
# the cache
# cachesolve(matrACache) # third call now calculates inverse again

makeCacheMatrix <- function(x = matrix()) {
    # Set inverse to NULL
    xinv <- NULL
    # Calling A$set will reset the cache value where A is a cached matrix.
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    # A$get returns the cached value.
    get <- function() x
    setinv <- function(solve) xinv <<- solve
    getinv <- function() xinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

## Cachesolve requires a "cachematrix" as an input.
# First it checks if an inverse has been calculated before.
# If so it returns the cached value.
# If not, it calculates the inverse and sets the value into
# the cache with x$setinv.

cachesolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Return NULL or inverse matrix
    xinv <- x$getinv()
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    # No inverse calculated before, so we get the value of x
    # for calculating the inverse.
    data <- x$get()
    # Calculate the inverse.
    xinv <- solve(data, ...)
    # Cache the value.
    x$setinv(xinv)
    # Return the inverse value.
    xinv
}
