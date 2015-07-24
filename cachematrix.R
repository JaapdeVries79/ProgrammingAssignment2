## Put comments here that give an overall description of what your
## functions do


## Git commit test by Jaap de Vries 7-24-2015


## Added by Jaap de Vries 7-24-2015
## This function creates a special "matrix" object that can cache its inverse.
## This main function stores 4 function, get, set, getinv, setinv, and puts them in a list
makeCacheMatrix <- function(x = matrix()) {
    
    ## sets the value of m to NULL
    inv <- NULL
    
    ## set is a function that changes the matrix stored in the main function.
    ## We don't need to use this function unless we want to change the matrix. 
    ## "x <<- y" substitutes the matrix x with y (the input) in the main function (makeCacheMatrix). 
    ## If it was "x <- y" it would have substitute the matrix x with y only in the set function. 
    ## "m <<- NULL" restores to null the value of the inverse m, because the old inverse of the old matrix 
    ## is not needed anymore. The new inverse needs to be recalculated through the function cacheSolve.
    set <- function(y) {
        x <<- y
        inv <<- matrix(NULL)
    }
    
    ## get is a function that returns the matrix x stored in the main function. 
    ## Doesn't require any input.
    get <- function() x
    
    ## setmean and getmean are functions very similar to set and get. 
    ## They don't calculate the mean, they simply store the value of the input in a variable m 
    ## into the main function makeVector (setmean) and return it (getmean).
    setinv <- function(invnew) inv <<- invnew
    
    ## simply returns the current inverse matrix
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv) 
}

## Added by Jaap de Vries 7-24-2015
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    ## The first thing cacheSolve does is to verify the value inv, stored previously with getmean, 
    ## exists and is not NULL. If it exists in memory, it simply returns a message and the value inv, 
    ## that is supposed to be the mean, but not necessarily
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## If it was the case, "return(inv)" would have ended the function. So everything that follows this if() 
    ## is a sort of else {}. data gets the matrix stored with makeCacheMatrix, inv calculates the inverse of 
    ## the matrix and x$setinv(inv) stores it in the object generated assigned with makeVector. 
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv   
}
