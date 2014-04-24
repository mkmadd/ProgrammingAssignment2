## These two functions are simple rewrites of the example makeVector() and 
## cachemean() functions given for the assignment.
## makeCacheMatrix() is a matrix version of makeVector() which is used to set()
## and get() a matrix value, then get and set its inverse.
## cacheSolve() is an interface with the getinverse() and setinverse() functions
## of makeCacheMatirx() - it gets the current inverse, and if it doesn't exist,
## calculates and sets it.

## Returns a list containing the functions set(), get(), setinverse(), and
## getinverse() to respectively set and get a matrix value, and set and get its
## inverse.

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


## Takes an object of the type returned by makeCacheMatrix() and calls its
## getinverse() function.  If the inverse exists, it returns it, otherwise it
## calculates the inverse and calls setinverse() before returning the inverse.

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