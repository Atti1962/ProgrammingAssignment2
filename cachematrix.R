## The 1st function "makeCacheMatrix" creates a special matrix object
## that can cache its inverse. 
## The 2nd function "cacheSolve" will create the inverse of the matrix  
## only if that inverse wasn't created previously

## This one creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix(1:4, nrow = 2, ncol = 2)) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This will create the inverse of the matrix only if that inverse 
## wasn't created previously

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting inverse matrix data")
                return(m)
        }
        data <- x$get()
        m <- solve(x)
        x$setinv(m)
        m
}
