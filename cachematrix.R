## cacheMatrix.R
## The purpose of the functions implemented in this file is to make sure
## the inverse of a matrix is calculated only once by caching the result for future
## calls instead of repeatedly computing it.


## makeCacheMatrix
## This function creates a "special matrix", which is really a list containing a 
## function to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inverseM <- NULL
    
    set <- function(y) {
        x <<- y
        inverseM <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setInverse <- function(myInverse) {
        inverseM <<- myInverse
    }
    
    getInverse <- function() {
        inverseM
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}


## cacheSolve
## This function calculates the inverse of the "special matrix" created with the
## function above (makeCacheMatrix). However it first checks to see if the inverse of
## the matrix was already calculated (and cached). If so, it returns the result from
## the cache. Otherwise, it calculates the inverse, stores the value in the cache for
## future calls (and returns the value).

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        return (inverse) ##returns inversed matrix from cache and halts execution of function
    }
    
    ##inverse not cached - calculating, caching and returning the value
    data <- x$get()
    inverse <- solve(data) ##calculate
    x$setInverse(inverse) ##cache
    inverse ##return
}
