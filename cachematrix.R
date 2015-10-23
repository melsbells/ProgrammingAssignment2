## This pair of functions caches the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # parameter m within this function's environment is set to NULL
  set <- function (y) {
    x <<- y # global variable x is set to the input (y)
    m <<- NULL # global variable m is set to NULL
  }
  get <- function() x # supplies the value x back
  setInverse <- function(solve) m <<- solve # global variable m is set to the inverse Matrix
  getInverse <- function() m # supplies the value m back
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse() # sets m to the inverse matrix of x
  if(!is.null(m)) {
    message("getting cached data") 
    return(m) # exits the function if m has already been set to inverse matrix, returns inverse matrix
  }
  data <- x$get()
  m <- solve(data, ...) # only solves for inverse matrix if it has not been calculated already
  x$setInverse(m) #sets the value of the inverse matrix to cache for next time
  m # returns the value of the inverse matrix
}