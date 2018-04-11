## This function, makeCacheMatrix creates an object called "matrix" that can cache it's inverse:

makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    set <- function(x) {
        m <<- x
        inv <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The following function calculates the inverse of the object "matrix" created with the above function. 
## It will check if the inverse of the matrix has already been calculated or not.
## If the inverse has already been calculated, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(m, ...) {
    inv <- m$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- m$get()
    inv <- solve(data)
    m$setinverse(inv)
    inv
}
