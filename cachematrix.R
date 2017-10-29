## DataScienceSpecialisation: Programming Assignment 2
## This file contains three functions that enable a user to cache a matrix and
## its inverse.
## 1. "makeCacheMatrix(x)" makes a copy of a matrix x in cache and creates 
## four functions for (1) setting value of x, (2) getting value of x,
## (3) setting value of inverser of x, and (4) getting value of inverse of x.
## 2. "cacheSolve(x)" returns inverse of cache matrix cx. cacheSolve only
## computes inverse of x, if inverse of x is not already stored in cache.
## 3. "testCacheSolve(x)" tests cacheSolve for a random matrix. If cacheSolve
## works as intended, testCacheSolve(x) returns TRUE and the runtime of first
## and second run of cacheSolve. For large matrices, the first runtime should
## be greater than the second runtime.

## "makeCacheMatrix(x)" makes a copy of a matrix x in cache and creates 
## four functions for (1) setting value of x, (2) getting value of x,
## (3) setting value of inverser of x, and (4) getting value of inverse of x.

makeCacheMatrix <- function(x = matrix()) {
        xinverse <- NULL
        set <- function(y) {
                x <<- y
                xinverse <<- NULL
        }
        get <- function() x
        setinv <- function(value) xinverse <<- value
        getinv <- function() xinverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## "cacheSolve(x)" returns inverse of cache matrix cx. cacheSolve only
## computes inverse of x, if inverse of x is not already stored in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinverse <- x$getinv()
        if(!is.null(xinverse)) {
                message("getting cached data")
                return(xinverse)
        }
        data <- x$get()
        xinverse <- solve(data, ...)
        x$setinv(xinverse)
        xinverse
}

## "testCacheSolve(x)" tests cacheSolve for a random matrix. If cacheSolve
## works as intended, testCacheSolve(x) returns TRUE and the runtime of first
## and second run of cacheSolve. For large matrices, the first runtime should
## be greater than the second runtime.

testCacheSolve <- function(dim=1000, range=1000, accuracy=1e-10) {
        # Make a matrix M with specified range and dimensions
        M <- matrix(runif(dim*dim, min=-range, max=range),dim,dim)
        # Create cache matrix functions
        CM <- makeCacheMatrix(M)
        # stop time while using cacheSolve twice. 
        # For large matrices, second time should be much quicker 
        time1 <- Sys.time()
        CMinverse1 <- cacheSolve(CM)
        time2 <- Sys.time()
        CMinverse2 <- cacheSolve(CM)
        time3 <- Sys.time()
        # Test if cacheSolve returned same output both times
        if (all(CMinverse1 == CMinverse2)) {
                # Test if output of cacheSolve is actually inverse
                inverseM <- solve(M)
                if (max(abs(inverseM - CMinverse2)) < accuracy) {
                        success <- TRUE
                }
                else {
                        success <- FALSE
                }
        }
        else {
                success <- FALSE
        }
        list(success, time2-time1, time3-time2)
}