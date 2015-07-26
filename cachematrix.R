## File contains functions for caching result of matrix inversion in repeated
## operations. At first create the "cache matrix" for your data with makeCacheMatrix
## and then as you need to calculate matrix inversion call cacheSolve with
## the "cache matrix" as argument.

## Creates a special "cache matirx", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the solve (matrix inversion in R)
## 4. get the value of the solve.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(inv) s <<- inv
    getsolve <- function() s
    list(set = set,get = get,setsolve = setsove,getsolve = getsolve)
}


## Calculates the solve (matrix inversion) of the special "vector" created with 
## the above function, but if it was calculated early just returns the cached result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if (!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
