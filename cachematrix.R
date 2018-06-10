## Put comments here that give an overall description of what your
## functions do

## Function creates special "matrix". which contains a list of functions that set
## and get the matrix value and also set and get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## Function checks to see if inverse already exists, and if so it extracts it
## If no inverse exists, it calculates it and then caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
