## This function creates an object that stores a matrix an its inverse.
## The object contains a list of four functions that allow 
##you to retrieve the array to invest, its inverse value, 
##as well as replace the array to invest and its inverse value


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


## The argument to this function is an object created with 
##the makeCacheMatrix function. 
##MatrixCachematrix retrieves the previously value stored 
##in the makeCacheMatrixand object, if the stored value was NULL, 
##calculates the inverse matrix and stores it in the object 
##through the makeCacheMatrix setsolve() function

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

