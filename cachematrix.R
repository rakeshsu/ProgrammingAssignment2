## makeCacheMatrix and cacheSolve are pair of functions that cache the 
## inverse of a matrix.

 

## This function creates a special "matrix" object that can cache its inverse.
## It provides a list of functions that can read or update the matrix input and 
## read and update the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setmatrixinverse <- function(inverse) inv <<- inverse
    getmatrixinverse <- function() inv
    
    # create a list of available functions that can be accessed by object of 
    # makeCacheMatrix
    list(set = set, get = get,
         setmatrixinverse = setmatrixinverse,
         getmatrixinverse = getmatrixinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getmatrixinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    
    # Default behavior of solve with matrix input is to compute it's inverse.
    inv <- solve(data, ...)
    
    # set the inverse using the x object function list and return the inverse.
    x$setmatrixinverse(inv)
    inv
}
