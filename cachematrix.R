## The program below is divided in 2 parts.
## 1) The program makeCacheMatrix keeps a list of values in memory
## 2) The program cacheSolve calculates the Inverse of an matrix if it is not cached already.

## The makeCacheMatrix takes in a matrix and returns a list
## which is used later used by the function cacheSolve.

makeCacheMatrix <- function(x=matrix(data=NA,nrow=1,ncol=1)) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inv) m <<- inv
  
  getInverse <- function() m
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The function cacheSolve takes in a matrix and checks if 
## the inverse of the matrix is already calculated or not by calling the 
## function makeCacheMatrix. If the values returned is Null it uses the SOLVE() in R
## to calculated the Inverse of a matrix and again called the makeCacheMatrix.setInverse() to save this value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	 m <- x$getInverse()
	  if(!is.null(m)) {
		message("getting cached data")
		return(m)
	  }
	  data <- x$get()
	  m <- solve(data, ...)
	  x$setInverse(m)
	  m
}
