## R Programming, Week 3, Assignment 2
## Submitted by Lance Andrewes, 26 July 2015
##
## The functions are used to cache the inverse of a matrix.
## My solution is heavily based on the makeVector and cachesolve
## samples supplied with the assignment instructions.
## Assumptions: the instruction say we can assume that the matrix
##              is always invertible.

## makeCacheMatrix creates a cached matrix object that is the inverse of
## the supplied matrix, for later retrieval by function cacheSolve.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                #  Operator '<<-' causes a search through parent
                #  environments for the variable being assigned.
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

## Function cacheSolve computes the inverse of the cached-matrix created by
## function 'makeCacheMatrix'.If the inverse has already been calculated then
## this function retrieves the inverse of the matrix from the cached-matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # cached inverse not found, so create the inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
