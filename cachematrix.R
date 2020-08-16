## Put comments here that give an overall description of what your
## functions do

## This script creates two functions makeCacheMatrix and cacheSolve
## The intent is to write a program that can cache the inverse of a matrix
## rather than compute it repeatedly. Since matrix inversion is a costly
## computation for large matrices/datasets, these functions may be useful

## Write a short comment describing this function
## This makeCacheMatrix function creates a special matrix that 
## caches the inverse of a given, input matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) inverse <<- Inv
        getInv <- function() inverse
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## The cacheSolve function computes the inverse of the special "matrix"
## returned by the makeCacheMatrix. If the inverse was previously computed,
## then the cacheSolve function should retrieve the inverse from the cache.
## This function will use the solve function in R to compute the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInv()
        if(!is.null(inverse)) { ## checks if inverse has been calculated
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInv(inverse)
        inverse
}
