## GOAL1:   makeCacheMatrix: creates a special "matrix" object that can cache its inverse
## GOAL2:   cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##                If the inverse has already been calculated (and the matrix has not changed), then
##                the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL     # Initializes the inverse property
      set <- function(matrix) {  # method to set the matrix
            x <<- matrix
            inv <<- NULL
      }
      get <- function() x  # method to get the matrix
      setinverse <- function(solve) inv <<- solve  # method to set the inverse of the matrix
      getinverse <- function() inv  # method to get the inverse of the matrix
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve
cacheSolve <- function(x, ...) {
      inv <- x$getinverse()  # Return a matrix that is the inverse of 'x'
      if(!is.null(inv)) {  # Just return the inverse if its already set
            message('getting cached data')
            return(inv)
      }
      data <- x$get()  # Get the matrix from our object
      inv <- solve(data, ...)  # Calculate the inverse using matrix manipulation
      x$setinverse(inv)  # Set the inverse to the object
      inv  # Return the matrix
}
