## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    so <- NULL
    set <- function(y)  {
        x <<- y
        so <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) so <<- solve
    getsolve <- function() so
    list(set = set, get = get, 
         setsolve = setsolve, 
         getsolve = getsolve)
    
    }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    so <- x$getsolve()
    if(!is.null(so))  {
        message("getting cached data")
        return(so)
    }
    data <- x$get()
    so <- solve(data, ...)
    x$setsolve(so)
    so
    
    }

