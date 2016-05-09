# makeCacheMatrix creates a list containing a function to



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
# 1. set the value of the matrix
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
}
  getmatrix <- function() x
# 2. get the value of the matrix
  
  setinverse <- function(solve) m <<- solve()
# 3. set the value of inverse of the matrix
  

  getinverse <- function() m
# 4. get the value of inverse of the matrix
  

  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## return: inverse of the original matrix input to makeCacheMatrix()
  
  inverse = x$getinverse()
  
  # if we already had calculations for x
  if (!is.null(inverse)){
    if(x$setmatrix() == x$getmatrix()) {
## get it from the cache and skips the computation. 
    message("getting cached data")
    return(inverse)
  }
  }
  
## if not, calculates the inverse 
  data = x$getmatrix()
  inverse = solve(data, ...)

## sets the value of the inverse in the cache via the setinverse function.
  x$setinverse(inverse)
  
  return(inverse)
}  ## Return a matrix that is the inverse of 'x'
