## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##this function sets up a list of functions to store and manipulate the cache 
makeCacheMatrix <- function(x = matrix()) {
    xSolve <- NULL
    set <- function(y) {  
        x <<- y         ### set function stores a new matrix 
        xSolve <<-NULL  ### nullify the previouse inverse
    }
    get <- function() x    ### return the original matrix
    setSolve <- function (xs) xSolve <<- xs    
    getSolve <- function() xSolve  # get the inverse
    list( set = set, get=get, setSolve=setSolve, getSolve=getSolve)
    
}


## This function takes in the list of functions  and uses them to manage the store
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xSolve <- x$getSolve()
    ### if the inverse is in the cache return it
    if (!is.null(xSolve)) 
    {
        message("getting cached data")
        return(xSolve)
    }
    ### inverse not cached then calculate it and 
    data <- x$get()
    ### calculate and store the inverse
    xSolve <- solve(data, ...)
    x$setSolve(xSolve)
    xSolve
    
}
