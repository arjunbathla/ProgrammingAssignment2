## These functions cache potentially time-consuming computation
## when calculating the inverse of a square matrix.


## makeCacheMatrix creates a special "matrix" which 
## is essentially a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve calculates the inverse of the special "matrix" 
## created with makeCacheMatrix. However it first checks to 
## see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates 
## the inverse of the matrix and sets the value of 
## the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
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
