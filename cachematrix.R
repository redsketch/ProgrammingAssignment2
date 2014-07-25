## A pair of functions that cache and computing
## the inverse of a square matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    matrix <- NULL
    
    ## A set of closure functions
    set <- function(y, ...) {
        x <<- matrix(y, ...)
        matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) matrix <<- inverse
    getinverse <- function() matrix
    testinverse <- function() round(matrix %*%x)
    
    ## Create a list of functions, so the closure function can be called outside
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse,
         testinverse = testinverse)

}


## This function computes the inverse of the special "matrix".

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Assign getinverse to matrix
    matrix <- x$getinverse()
    
    ## If matrix does not null, get the inverse from cached and then pass/return it
    if(!is.null(matrix)) {
        message("getting cached data")
        return(matrix)
    }
    
    ## Solve the inverse
    data <- x$get()
    matrix <- solve(data)
    x$setinverse(matrix)
    matrix
}
