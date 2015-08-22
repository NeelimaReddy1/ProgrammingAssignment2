## Set the value of the matrix
## get the value of the matrix
## set the value of inverse of the matrix
## get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
             x <<- y
             inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function returns the inverse of the matrix. First checks if the inverse has already been computed. 
## If computed, it gets the results. If not, it computes the inverse, sets the value in cache using setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
           message("getting cached matrix")
           return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
}
