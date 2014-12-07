## Functions to calculate inverse of a matrix in an optimized way, 
## by caching the inverse, and the second time it is needed retreive it from cache, 
## avoiding unneccessary computation.
## Author: Rill Robert (December, 2017)

## creates a special "matrix", which is an object with getter 
## and setter methods for the matrix data itself and its inverse.
makeCacheMatrix <- function(X = matrix()) {
        inv <- NULL              # inv will be the inverse, initialize with NULL
        set <- function(Y) {     # reset the matrix and inverse with the superassignment operator
                X <<- Y          # the new matrix will be Y
                inv <<- NULL     # the inverse is NULL again (for a new matrix)
        }
        get <- function() X      # return the current matrix
        setInverse <- function(inverse) inv <<- inverse   # set the inverse to the parameter
        getInverse <- function() inv                      # return inverse of current matrix
        list(set = set, get = get,       # the function returns a list of the 
             setInverse = setInverse,    #    getter and setter methods
             getInverse = getInverse)    #    for the matrix and its inverse
}


## calculates the inverse of the special "matrix" object created 
## with the above function and caches the result.
## If inverse has already been calculated, get it from the cache 
## and skips the computation. 
## Otherwise, calculates inverse and sets stores it in cache.
cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'X'
        inv <- X$getInverse()                   # get inverse of object
        if(!is.null(inv)) {                     # if inverse was already cached (not NULL)
                message("getting cached data")  # display message
                return(inv)                     # and return the inverse
        }                                       # OTHERWISE
        data <- X$get()                         # get matrix of the local X object
        inv <- solve(data)                      # calculate inverse of matix
        X$setInverse(inv)                       # set the inverse for the object
        inv                                     # inverse is returned by this function
}
