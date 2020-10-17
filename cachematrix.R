# The functions in this source code are for 
# the Assignment: Caching the Inverse of a Matrix

## This function creates a list of functions for creating a cache matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      get <- function() x
      
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This function solves for the inverse of a matrix (assuming it has 
# not yet been computed before) and returns the value 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)){
            message("getting cached data...")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
