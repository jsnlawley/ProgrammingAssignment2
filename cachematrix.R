## GOAL1:   makeCacheMatrix: creates a special "matrix" object that can cache its inverse
## GOAL2:   cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##                If the inverse has already been calculated (and the matrix has not changed), then
##                the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
      
      # Initializes the inverse property
      inv <- NULL
      
      # method to set the matrix
      set <- function(matrix) {
            x <<- matrix
            inv <<- NULL
      }
      # method to get the matrix
      get <- function() x
      # method to set the inverse of the matrix
      setinverse <- function(inverse) inv <<- inverse
      # method to get the inverse of the matrix
      getinverse <- function() inv
      
      # returns a list of the methods
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve
cacheSolve <- function(x, ...) {
      
      # Return a matrix that is the inverse of 'x'
      inv <- x$getinverse
      
      # Just return the inverse if its already set
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      # Get the matrix from our object
      data <- x$get
      
      # Calculate the inverse using matrix manipulation
      inv <- solve(data,...)
      
      # Set the inverse to the object
      x$setinverse(inv)
      
      # Return the matrix
      inv
}
