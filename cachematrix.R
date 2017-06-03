## R Programming Assignment 2
## These functions create special matrix that can cache its own inverse
## and solve for the inverse of such a matrox

## Returns a cacheable wrapper of a given matrix which can return a previously calculated inverse
## of the matrix if it exists

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Returns the  inverse of a cacheable matrix. If the cache exists it is returned,
## otherwise the inverse is calculated, placed in the cache, and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getinverse()
        
        if(!is.null(inverse)) {
                message("getting cached inverse data")
                return(inverse)
        }
        
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
