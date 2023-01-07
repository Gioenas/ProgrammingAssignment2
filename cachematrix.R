## My goal is to have this functions load to the cache the inverse of a given 
## matrix
## makeCaxheMatrix, will set the original matrix in the cache and possibly
## calling setinv directly set the inverse of the matrix in the cache
## calling getinv gets back from the cache the inverse of the matrix
## while calling get will retrieve the original matrix



makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverted) m <<- solve(inverted)
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



## This function "casheSolve", will first retrieve the inverse of the original
## matrix, then check if is stored in the cache. If the inverse of the matrix, is
## in the cache will retrieve and print a message "getting cashed data", if is
## not in the cache will get the original matrix with the functions, supplied by
## the object returned from "makeCacheMatrix" and set the inverse in the cache.
## In addition will return a message "storing new data to cache"

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    message("storing new data to cache")
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
   
}


