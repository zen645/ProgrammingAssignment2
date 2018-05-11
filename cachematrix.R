## Program will create a cache of a matrix and store the inverse 

makeCacheMatrix <- function(m = matrix()){
    inv <- NULL
    set <- function (m1){
        m <<- m1
        inv <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Program will solve for the inverse of the matrix and if there is a null 
## matrix return the cache inverse.
cacheSolve <- function(m, ...){
    inv <- m$getinv()
    
    ## Checks to see whether the result is null.  If the value here is not equal 
    ## to null the inverse will be returned
    if(!is.null(inv)){
        message("Getting cached data")
        return(inv)
    }
    ## assign the matrix to the data variable
    data <- m$get()
    ## This solves the inverse of a matrix
    inv <- solve(data)
    m$setinv(inv)
    inv
}