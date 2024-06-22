## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # Initialize the inverse property
        
        # Function to set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL  # Reset the inverse when the matrix is reset
        }
        
        # Function to get the matrix
        get <- function() {
                x
        }
        
        # Function to set the inverse
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        
        # Function to get the inverse
        getInverse <- function() {
                inv
        }
        
        # Return a list of the above functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        
        # If the inverse is already calculated, retrieve it from the cache
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Otherwise, calculate the inverse
        data <- x$get()
        inv <- solve(data, ...)
        
        # Cache the inverse
        x$setInverse(inv)
        
        # Return the inverse
        inv
}