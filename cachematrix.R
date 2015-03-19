##Builds a function which functions like a matrix 
##for purposes of basic interactions
##but stores its inverse to save compute time



## Constructor for the cached matrix. 
## Takes a square matrix as input, and stores 
## the inverse once computed via cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  I = NULL
  set <- function(y) {
   x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) I <<- solve
  getInverse <- function() I
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
  



## Takes a cache matrix and computes the inverse
## if required, or returns the previous inverse if cached

cacheSolve <- function(x, ...) {
  
    I <- x$getInverse()
    if(!is.null(I)) {
      message("getting cached data")
      return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setInverse(I)
    return(I)
}
