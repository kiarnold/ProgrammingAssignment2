## This pair of functions will return the inverse of a matrix.
##
## They are optimized so that this calculatino will only occur once for any given matrix.
## If a new matrix is passed to the functions, the will calculate and store the inverse.
## After this, the functions will merely return the stored inverse result, saving time.
##
## To use: assume your matrix is "m"
## Call: cacheSolve(makeCacheMatrix(), m)... 

## This function creates a special "matrix" object that can cache its inverse.
## The whole functions returns a list with each of the four functions in it.
##
## "set" will change the matrix in question and reset the cached inverse
## "get" will return the matriz in question
## "setInverse" will set the inverse of the matrix by solving x
## "getInverse" will return the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        matrixInverse <- NULL
        
        set <- function(y) {
                x <<- y
                matrixInverse <<- NULL
        }
        calculateInverse <- function() matrixInverse <<- solve(x)
        getInverse <- function() matrixInverse
        
        list(set = set, calculateInverse = calculateInverse, getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve 
## the inverse from the cache.
## 
## Function looks up the cached inverse. If that is no null, it returns it. 
## Otherwise, the function will calculate and store the matrix, and then it
## will return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("Got cached data.")
                return(inverse)
        }
        
        message("Matrix not cached, calculating...")
        
        x$calculateInverse()
        x$getInverse()
}
