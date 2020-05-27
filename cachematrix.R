#As the assignment states, assuming that the matrix supplied is always invertible.
#Import library to use function inv() to calculate inverse of a matrix
library(matlib)
## Function makeCacheMatrix() creates an R object that stores a matrix and its inverse.
#1.set: set the value of a matrix
#2.get: get the value of a matrix
#3.setInverse: set the matrix inverse using function inv from the library matlib
#4.getInverse: get the matrix inverse using function inv from the library matlib
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) m <<- inv(x)
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
#Function cacheSolve retrieve the inverse of the matrix 
#from the cached value stored in the makeCacheMatrix()'s environment
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inv(data, ...)
        x$setInverse(m)
        m
}