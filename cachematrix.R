## Description: This program contains two functions of 
##              makeCacheMatrix and CacheSolve, that
##              would calculate and cache the inverse 
##              of a matrix.

## makeCacheMatrix function, creates a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invert <- NULL
    set <- function(m) {
        x <<- m
        invert <<- NULL
    }
    get <- function() x
    setinvert <- function(solve) {
        invert <<- solve
    }
    getinvert <- function() invert
    list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)
}

## cacheSolve function, obtains/gets the inverted value of
## inputted matrix from the makeCacheMatrix function. Then,
## if matrix has already been calculated, it would print the
## message of "getting cached data" along with the inverted
## value, otherwise it would calculate the invert value to
## be printed.

cacheSolve <- function(x, ...) {
    invert <- x$getinvert()
    if(!is.null(invert)) {
        message("getting cached data")
        return(invert)
    }
    data <- x$get()
    invert <- solve(data, ...)
    x$setinvert(invert)
    invert
}