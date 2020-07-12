## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y){
                x <<- y
                I <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) I <<- inverse
        getInverse <- function() I 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        I <- x$getInverse()
        if(!is.null(I)){
                message("getting cached data")
                return(I)
        }
        mat <- x$get()
        I <- solve(mat,...)
        x$setInverse(I)
        I
}
