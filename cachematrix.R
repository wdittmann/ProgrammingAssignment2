## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    xSolve <- NULL
    set <- function(y) {
        x <<- y
        xSolve <<-NULL
    }
    get <- function() x
    setSolve <- function (s) xSolve <<- solve(s)
    getSolve <- function() xSolve
    list( set = set, get=get, setSolve=setSolve, getSolve=getSolve)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xSolve <- x$getSolve()
    if (!is.null(xSolve))
    {
        message("getting cached data")
        return(xSolve)
    }
    data <- x$get()
    xSolve <- solve(data, ...)
    x$setSolve(xSolve)
    xSolve
    
}
