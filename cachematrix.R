## Put comments here that give an overall description of what your
## functions do:

## (The functions are totally analog to the vector example given in the
## instructions to this assignment)

## makeCacheMatrix creates and returns a list of functions for
## getting and setting the matrix and its inverse, resp.
## cacheSolve takes such a list and computes the inverse in case it
## is not already available and the original matrix has not changed.

## Write a short comment describing this function:
## makeCacheMatrix takes a matrix as inital input to store and
## returns a list of functions to set and get the matrix, and to
## set and get its inverse.
## if a new matrix is set (via the set function), the cached inverse
## is set to NULL such that it will be recomputed.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function:
## cacheSolve computes the inverse of a matrix represented in terms of
## the list of functions created by the makeCacheMatrix function.
## If the inverse is already existing, it will be returned immediately,
## if not, it will be computed and stored via setinverse().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
