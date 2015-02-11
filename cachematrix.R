## makeCacheMatrix creates a 'matrix' object in the form of a list.
## This object allows you to get/set a matrix as well as get/set
## the inverse of that matrix. It stores the inverse of the matrix
## as a member variable so it's cached and doesn't have to be recomputed
## when needed multiple times.

## cacheSolve either computes the inverse of the matrix in the 
##above-mentioned 'matrix' object and stores it in the 
## cache variable or it returns the contents of that cache if
## the inverse was previously computed.

## Creates the special matrix object and its respective functions
makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse)i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse of the matrix. It either pulls it from
## the cache or calculates the inverse and stores it in the cache
## for future function calls
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i<- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i<-solve(data)
    x$setinverse(i)
    i
}
