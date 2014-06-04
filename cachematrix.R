## Assignment 2: Caching the inverse of a Matrix
###Name: Andy
###Class: R @ Coursera

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    #initialize inverse
    inverse <- NULL
    
    #object setter
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    
    #object getter
    get <- function() x
    
    #set inverse
    setInverse <- function(inverse_arg) inverse <<- inverse_arg
    
    #get inverse
    getInverse <- function() inverse
    
    #prints information of the cached object
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
### If the inverse has already been calculated (and the matrix has not changed), then casheSolve
### should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    #get inverse of x
    inverse <- x$getInverse()
    
    #execute this block of code if x$inverse is not null
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    
    #at this point, we know that inverse for given matrix is still null
    #get the actual matrix
    data <- x$get()
    
    #calculate the inverse for the matrix
    inverse <- solve(data, ...)
    
    #setting the inverse for this matrix
    x$setInverse(inverse)
    
    #returns the inverse matrix
    inverse
}