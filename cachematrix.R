## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix function will contain
# getter and setter methods for the matrix and the inverse stored in the cache

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverseMatrix <<- inverse
  getInverse <- function() inverseMatrix
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


# The following function returns the inverse of the matrix.  

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  # if matrix is computed, return from function with the value
  if(!is.null(inverseMatrix)) {
    message("getting cached data.")
    return(inverseMatrix)
  }
  # else calculate the inverse using the setinverse function and set cache.
  data <- x$get()
  inverseMatrix <- solve(data)
  x$setinverse(inverseMatrix)
  inverseMatrix
}
