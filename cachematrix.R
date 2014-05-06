## This script defines functions to create and cache inverse of a matrix.

## A special type of matrix that returns a list consisting of functions
## to get and set matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL #the inverse of matrix x
  
  set <- function(y) { # set the matrix.
    x <<- y
    # as the inverse has not been caculated, 
    # set inverse to NULL
    inverse <- NULL 
  }
  get <- function() { #return the original matrix x
    x
  }
  setInverse <- function(z) { #set inverse of matrix x 
    inverse <<- z
  }
  getInverse <- function() { #get inverse of matrix x
    inverse
  }
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Function that caches the inverse of a matrix
## The function argument x is the matrix returned by the makeCacheMatrix
## function.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse() #get the inverse of the matrix x.
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse) #return inverse if available in the cache
  }
  
  data <- x$get() #get the actual matrix whose inverse we need to calculate
  inverse <- solve(data) # generate inverse
  x$setInverse(inverse)
  inverse
}
