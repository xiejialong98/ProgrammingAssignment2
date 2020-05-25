## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Set and get for the original data
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  # Set and get for the inversed matrix
  setinv <- function(inversed) inv <<- inversed
  getinv <- function() inv
  
  # The list of the functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # Return the inversed matrix if it is already there
  if(!is.null(inv)) { 
    message("getting cached inversed matrix")
    return(inv)
  }
  data <- x$get() # extract the matrix
  inv <- solve(data, ...) # Solve to get inversed matrix
  x$setinv(inv) # set the inversed matrix
  inv ## Return a matrix that is the inverse of 'x'
}