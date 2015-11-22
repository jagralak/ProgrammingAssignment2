## This file defines two functions
## - makeCacheMatrix()
## - cacheSolve()
## used for CPU-efficient inverse matrix computation.

## Example usage:
## > source("cachematrix.R")
## > mymatrix <- matrix(c(1,-0.25,-0.25,1), nrow=2,ncol=2)
## > mymatrix
##      [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
## > fvec <- makeCacheMatrix(mymatrix)
## > cacheSolve(fvec)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > cacheSolve(fvec)
## getting cached data
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 


## makeCacheMatrix() function initiates a vector of functions used 
## for matrix inversion process. See description of cacheSolve()
## for the further details.
makeCacheMatrix <- function(x = matrix()) {
  ## Initiate local cache (i.e. the place were inverted matrix is stored)
  m <- NULL

  ## set matrix and reset the local cache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get/return matrix
  get <- function() x
  ## set cached matrix
  setsolve <- function(solve) m <<- solve
  ## return cached matrix
  getsolve <- function() m
  ## return vector of functions used for cached matrix solving
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve() is a wrapper funtion around solve(), which attempts
## to speed up matrix inversion process by looking up a result of
## the previous calls. If such previous catched inverted matrix is found
## it is returned with a corresponding "getting cached data" message 
## and no further computation takes place. Should, no catched inverted
## matrix be found, a new computation takes place and its result prior
## to function return is stored in the cache.
cacheSolve <- function(x, ...) {
  ## Check if there is anything in cache, if yes
  ## return that result and do not go further
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## This is the first computation (cache was empty).
  ## Get data, "solve" it and store the result (setsolve)
  ## for later use.
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  ## return inverted matrix
  m
}
