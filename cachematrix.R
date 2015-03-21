## Functions in this module handle a matrix and keep its inverse cached to
## optimize computations when the inverse is accessed often.

## Returns a cached matrix together with the functions that grant access to it,
## to its inverse, and that allow the matrix to be changed and a new inverse
## value to be stored in the cache.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
  
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
  
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
  
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Acceses the cached value of the inverse of the matrix received as
## argument and if not NULL returns it, otherwise computes its inverse,
## stores it in the cache and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get ()
    
    m <- solve(data, ...)
    x$setsolve(m)
    
    m
}

