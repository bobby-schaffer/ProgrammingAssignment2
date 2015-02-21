## cachematrix.R contains a pair of functions to support
##  caching the inverse of a matrix rather than computing it repeatedly
  

## This function creates a special "matrix" object that can cache its inverse
## It includes a list of 4 exposed functions to get, set, setinverse and getinverse
## These functions operate on the square matrix "x"  that is the function argument
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


##This function computes the inverse of the "matrix" object returned by makeCacheMatrix. 
##If the inverse has already been calculated (and the matrix has not changed),
##then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m    
}
