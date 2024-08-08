## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # initialize the inverse as NULL
  
  # setter matrix
    set <- function(y) {
    x <<- y  # new matrix
    inv <<- NULL  # rest inverse cache
  }
  
  # getter matrix
  get <- function() {
    x
  }
  
  # setter inverse
  setInverse <- function(inverse) {
    inv <<- inverse  # cache the inverse
  }
  
  # getter inverse
  getInverse <- function() {
    inv  
  }
  
  # Return the list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  # check if the inverse is already cached
  if (!is.null(inv)) {
    message("getting cached data")  # inform cached data is being used
    return(inv) 
  }
  
  # if not cached, get the matrix and compute the inverse
  data <- x$get()  
  inv <- solve(data, ...)
  x$setInverse(inv)  # Cache the inverse for future use
  
  inv 
}
