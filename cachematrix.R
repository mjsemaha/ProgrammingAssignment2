## These two functions save time when the inverse of the same matrix is needed 
## many times. the inverse is calculated once, stored, and reused until the 
## matrix changes


## makeCacheMatrix= stores a matrix together with a cached copy of its inverse
## and returns a list of four functions to set/get the matrix and set/ get the 
## inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
}
        get <- function() x
        setinverse <- function (inverse) inv <<- inverse
        getinverse <- function () inv
        list( set =set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: returns the inverse of a matrix made with makeCacheMatrix. IF
## the inverse is already cashed it uses that, otherwise it compute it with
## solve() and saves it for next time.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return (inv)
        }
        data <- x$get()
        inv <- solve (data, ...)
        x$setinverse (inv)
        inv
}
