## These functions together store a matrix and cache its inverse since matrix
## inversion is a computatinally costly process

## Creates a matrix object and caches its inverse
makeCacheMatrix <- function(original_matrix = matrix()) {
    
    #Initialize the inverse matrix to NULL
    inverse_matrix <- NULL
    
    #Set the value of the matrix
    set <- function(y) {
        original_matrix <<- y
        inverse_matrix <<- NULL
    }
    
    #Get the value of the matrix
    get <- function() original_matrix
    
    #Set the inverse of the matrix with the built in solve function
    setinverse <- function(solve) inverse_matrix <<- solve
    
    #Get the inverse of the matrix
    getinverse <- function() inverse_matrix
    
    #Return a list containing all the above functions
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## Computes the inverse of the special matrix object from above
## If the inverse has already been stored, it simply retrieves the answer
## from the cache
cacheSolve <- function(cache_matrix, ...) {
    
    #Get the inverse of the matrix
    inverse_matrix <- cache_matrix$getinverse()
    
    #If the inverse has already been stored, return the cached value
    if (!is.null(inverse_matrix)) {
        message('Getting Cached Data')
        return(inverse_matrix)
    }
    
    #Otherwise compute the inverse, store it in the cache, and return it
    data <- cache_matrix$get()
    inverse_matrix <- solve(data)
    cache_matrix$setinverse(inverse_matrix)
    inverse_matrix
}
