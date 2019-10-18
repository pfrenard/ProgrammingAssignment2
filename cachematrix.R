## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix create a special Matrix with ability to cache its inverse Matrix
## makeCacheMatrix have an optional matrix parameter
## set function stores the matrix
## get function gives back the matrix
## setInverse function stores the inverse matrix
## getInverse function gives back the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    list( set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}


## Write a short comment describing this function
## cacheSolve is a function that calculates the inverse matrix of the one in parameter
## if inverse matrix is allready in cache, just return this matrix, if not calculate it for real
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("getting cached inverse matrix")
    } else {
        message("calculating inverse matrix")
        inverse <- solve(x$get(), ...)
        x$setInverse(inverse)
    }
    inverse
}
