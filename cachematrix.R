##The two functions makeCacheMatrix() and cacheSolve() are designed to compute the inverse of a matrix 
## and store it in cache to save on potentially time-consuming computations.
## The idea here is for user to retrieve the inverse of matrix if it is stored already in 
## cache. If not it can compute it and store in cache for subsequent use.

## makeCacheMatrix() creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  set <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) xInv <<- solve(x)
  getInverse <- function() xInv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve() function returns the inverse of the matrix passed as an argument.
##It first checks if the inverse if already computed, else it computes it 
## and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xInv <- x$getInverse()
  if(!is.null(xInv)) {
    message("getting cached data")
    return(xInv)
  }
  data <- x$get()
  xInv <- solve(data, ...)
  x$setInverse(xInv)
  xInv
  
}
