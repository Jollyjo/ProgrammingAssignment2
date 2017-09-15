## Set of functions for compute and caching the inverse of a matrix

## Function to create a list of sub-function to:
## Set the Matrix, Get the Matrix, Set Inverse of the matrix, Get an Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
        minverse <- NULL
        set <- function(y) {
                x <<- y
                minverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) minverse <<- inverse
        getinverse <- function() minverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## Function to return an Inverse a Matrix (From Cache if it exists already)

cacheSolve <- function(x, ...) {
        minverse <- x$getinverse()
        if(!is.null(minverse)) {
                message("getting cached data")
                return(minverse)
        }
        mat.data <- x$get()
        minverse <- solve(mat.data, ...)
        x$setinverse(minverse)
        return(minverse)
}