## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # to store the inverse

    set <- function(y) {
        x <<- y
        inv <<- NULL  # reset the inverse if the matrix is changed
    }

    get <- function() x  # return the matrix

    setinverse <- function(inverse) inv <<- inverse  # store the inverse

    getinverse <- function() inv  # return the inverse

    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated and the matrix has not changed,
## it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()

    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }

    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}

