
# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it repeatedly 
# Here, we are writing functions to cache the inverse of a matrix


# makeCacheMatrix returns a list containing the following functions:
# 1. setMatrix  - set the value of the matrix
# 2. getMatrix  - get the value of the matrix
# 3. setInverse - set the value of inverse of the matrix
# 4. getInverse - get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    # Nothing is cached at the start..set cached inverse to NULL 
    cachedInv <- NULL
    
    # Store the matrix
    setMatrix <- function(newVal) {
        x <<- newVal
        # Since matrix is set to a new value, clear the cache
        cachedInv <<- NULL
    }
    
    # Return the stored matrix
    getMatrix <- function() {
        x
    }
    
    # Cache the computed Inverse
    setInverse <- function(solvedInv) {
        cachedInv <<- solvedInv
    }
    
    # Return the cachedInv     
    getInverse <- function() {
        cachedInv
    }
    
    # Return the list of functions
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

# The cacheSolve function returns the inverse of the matrix. 
# It first checks if the inverse has already been computed and cached
# If yes, it skips the computation and gets the inverse from the cache
# If not, it computes the inverse and sets the value in the cache 
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {

    # get the cached inverse of matrix.
    inv <- x$getInverse()
    
    # If the cache was not NULL, then return the cached Inverse 
    if(!is.null(inv)) {
        message("Returning the inverse from the cache..")
        return(inv)
    }
    
    # Else, compute the inverse, store it in the cache and return the inverse
    data <- x$getMatrix()
    inv <- solve(data)
    x$setInverse(inv)
    inv        
}
