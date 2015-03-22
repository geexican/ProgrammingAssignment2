## Two functions, one function creates a matrix 
## the other solves the matrix but uses a cached value if available

## this function makes a matrix and helper functions for finding the inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve 
        getinv <- function() m
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## used to solve a matrix created with makeCacheMatrix, if available
## this function will return a chached inverse, otherwise it will 
## compute a new value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message('getting cached data')
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
