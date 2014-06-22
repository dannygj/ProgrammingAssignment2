#This function establishes a list of 4 functions. Using the "set" function establishes a matrix and using the "get" function retrieves that matrix. 
#The "setinverse" function establishes the inverse functionality and the "getinverse" function calls the inverse function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#Once a matrix is defined using the "set" function running CacheSolve (below) on makeCacheMatrix (above) returns the inverse of the matrix. It then stores that inverse in the cache. If the function is called again, rather than recalculating the inverse it returns the previously stored value.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}