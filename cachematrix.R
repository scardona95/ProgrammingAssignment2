#makeCacheMatrix is a function which creates a special "matrix" object 
#that can cache its inverse for the input (which is an invertible square matrix)

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize the inverse property
    inv <- NULL
    
    ## Method to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Method the get the matrix
    get <- function() (x)
    
    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {inv <<- inverse}
    
    ## Method to get the inverse of the matrix
    getInverse <- function() {inv}
    
    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#cacheSolve is a function which computes the inverse of the special "matrix"
#returned by makeCacheMatrix above. If the inverse has already been calculated
#(and the matrix has not changed), then the cachesolve should retrieve the
#inverse from the cache.

cachesolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## Get the matrix from our object
    mat <- x$get()
    
    ## Calculate the inverse using matrix multiplication
    inv <- solve(mat, ...)
    
    ## Set the inverse to the object
    x$setInverse(inv)
    
    ## Return the matrix
    inv
}
