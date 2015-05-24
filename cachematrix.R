## These functions take an invertible matrix (given as the argument)
## and returns its inverted matrix. If it has been done before, then 
## it will return the cached inverted matrix. 

## The first part of this function serves to create a special matrix
## which can cache its inverse. 

makeCacheMatrix <- function(x = numeric()) {
        inv <- NULL 
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x  
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, 
             setinv = setinv, 
             getinv = getinv)
}

#This function takes whatever has been created in makeCacheMatrix (use that as argument)
#and then returns the inverse matrix. 
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
