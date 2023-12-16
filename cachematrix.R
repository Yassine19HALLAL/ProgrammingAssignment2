## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function to create a special "matrix" object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse matrix
    inv <- NULL
    
    # Setter function to set the matrix
    set <- function(newValue) {
        x <<- newValue
        inv <<- NULL  # Invalidate the cache when the matrix is updated
    }
    
    # Getter function to get the matrix
    get <- function() x
    
    # Function to cache the inverse of the matrix
    cacheInverse <- function() {
        if (!is.null(inv)) {
            message("Getting cached inverse")
            return(inv)
        }
        message("Calculating inverse and caching")
        inv <- solve(x)
        return(inv)
    }
    
    # Return a list of functions
    list(set = set, get = get, cacheInverse = cacheInverse)
}

# Function to compute the inverse of the matrix from makeCacheMatrix (caching if possible)
cacheSolve <- function(x) {
    inv <- x$cacheInverse()
    return(inv)
}
