# Below are two functions that are used to create a special 
# object that stores a numeric matrix and cache's its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # The function makeCacheMatrix() creates a special "matrix", 
    # which is really a list containing a function to
    # --set the value of the matrix
    # --get the value of the matrix
    # --set the value of the inverse of the matrix
    # --get the value of the inverse of the matrix
    # This function creates a special "matrix" object that can cache its 
    # inverse.
    x_inv <- NULL
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) x_inv <<- inv
    getinverse <- function() x_inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


cacheSolve <- function(x, ...) {
    # The function cacheSolve() computes the inverse of the special "matrix" 
    # returned by makeCacheMatrix() function. If the inverse has already been 
    # calculated and the matrix has not changed, then cacheSolve() should 
    # retrieve the inverse from the cache.
    # We assume that x is always invertible.
    x_inv <- x$getinverse()
    if(!is.null(x_inv)) {
        message("getting cached data")
        return(x_inv)
    }
    data <- x$get()
    x_inv <- solve(data)
    x$setinverse(x_inv)
    return(x_inv)
    ## Return a matrix that is the inverse of 'x'
}

# testing example
# x <- matrix(c(12,2,4,65,7,86,8,46,7), nc = 3)
# x
# solve(x)
# solve(x)%*%x
# makeCacheMatrix(x)
# cacheSolve(makeCacheMatrix(x))
# y <- makeCacheMatrix(x)
# x_inv <- y$getinverse()
# x_inv
# data <- y$get()
# x_inv <- solve(data)
# x_inv
# y$setinverse(x_inv)
# y$getinverse()
# cacheSolve(makeCacheMatrix(x))
# cacheSolve(y)


