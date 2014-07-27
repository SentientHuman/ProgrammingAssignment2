## Together, these two functions can be used to return the inverse of a matrix.
## If the inverse has not yet been calculated, they will do so.
## Once calculated, the inverse is cached. 
## If it is requested again, the functions retrieve that information
## Rather than recalculating. 

## makeCacheMatrix provides tools that allow for setting the value of the matrix,
## retrieving that value, setting the inverse, and retrieving it.
## Values are stored in the parent environment rather than within the function.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve checks whether an inverse has already been calculated
## if no existing inverse is cached, it performs the calculation.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
