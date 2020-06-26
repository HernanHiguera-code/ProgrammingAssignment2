## Put comments here that give an overall description of what your
## functions do

## the makeCacheMatrix function creates a special Matrix object to save the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        set_inverse <- function(inverse) {
                inv <<- inverse
        }
        get_inverse <- function() {inv}
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## the cacheSolve function return the cached inverse of the matrix if it exists, if not then inverted
# the matrix and store the result in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$set_inverse(inv)
        inv
}
