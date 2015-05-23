## makeCacheMatrix creates a special matrix object which is
## capable of caching the inverse of an invertible matrix.
## This is specially useful since matrix inversion is a resource
## consuming operation. Steps of the function are as follows;
## 1. If a new matrix 'y' is received it will be assigned to x
##     and reset cache
## 2. Defines the get function to return the original matrix
## 3. Defines the set function for the inverse matrix. This will
##     cache the inverse of the matrix.
## 4. Defines the get function for the inverse matrix
##
##
## cacheSolve will return the inverse of a given
## invertivle matrix. Steps as follows;
## 1. Check if the inverse matrix is available in
##     the cache. If found return the inverse matrix
## 2. Else inverse will be calculated, cached and
##     then will return.


## Creates a special matrix object which caches
## the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInv <- function (inverse) i <<- inverse
        getInv <- function () i
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## Returns an inverse of a given matrix

cacheSolve <- function(x = matrix(), ...) {
        i <- x$getInv()
        if (!is.null(i)) {
                message("Getting cached data")
                return (i)
        }
        data <- x$get()
        i <- solve(data)
        x$setInv(i)
        i
}
