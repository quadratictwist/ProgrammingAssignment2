## makeCacheMatrix creates a vector of funcitons associated to a 
## matrix x that can store the inverse of x in the cache. cacheSolve
## returns the inverse of x. It will use the cached value to avoid
## repetition, or otherwise store the inverse in the cache.

## makeCacheMatrix creates a vector of four functions given matrix x. 
## set(y) assigns y to x in the cache, and set inv as NULL to 
## initiate a vector with matrix y.
## get() returns the matrix x associated to the vector
## setinv() assigns the known inverse to inv in the cache
## getinv() retrns the inv stored in the cache

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) {
        inv <<- inverse
    }
    getinv <- function() inv
    list(set = set, get = get, 
         setinv = setinv, getinv = getinv)
}

## cacheSolve will return the inverse of x. If inv is in the cache, 
## casheSolve returns the inv stored in the cache. Otherwise it 
## calculates and returns the inverse of x and use setinv to store
## it in the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
