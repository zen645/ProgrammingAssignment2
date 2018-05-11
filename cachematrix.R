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

cacheSolve <- function(m, ...){
    inv <- m$getinv()
    
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