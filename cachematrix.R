##Builds a function which functions like a matrix 
##for purposes of basic interactions
##but stores its inverse to save compute time



## Constructor for the cached matrix. 
## Takes a square matrix as input, and stores 
## the inverse once computed via cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  I = NULL ##sets the inverse as null
  set <- function(y) {
   x <<- y ##setter method
  }
  get <- function() x ##getter method
  setInverse <- function(solve) I <<- solve ##setter for cache
  getInverse <- function() I ##getter for cache
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) ##list of methods
}
  



## Takes a cache matrix and computes the inverse
## if required, or returns the previous inverse if cached

cacheSolve <- function(x, ...) {
  
    I <- x$getInverse() ##gets the cached inverse
    if(!is.null(I)) { ##if it's not null, it was written to
      message("getting cached data")
      return(I) ##return the value and terminate
    }
    data <- x$get() ##otherwise get the matrix
    I <- solve(data, ...)  ##invert it
    x$setInverse(I) ##store the inverse
    return(I) ##return the inverse 
}
