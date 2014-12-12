## Functions to cashe matrix's inverse values:
## Below are two functions that are used to create a special object that stores
## a numeric matrix and caches its inverse value. 
## It is assumed what only invertable matrixes are used 
## as argument to a makeCacheMatrix function.


## Cache input matrix passed as arguments and return list of functions:  
## * set the value of the matrix
## * get the value of the matrix
## * setinv the function to set inverse matrix
## * getinv the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invM) inv <<- invM
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Calculate and return the inverse of the matrix wrapped by makeCacheMatrix. 
## Return cached value of the matrix if it was cached in previous calculations. 
cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}