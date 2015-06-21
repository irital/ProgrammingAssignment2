## These functions cache the inverse of a matrix if existant 
## and calculate it if not.

## The makeCacheMatrix function creates a list of functions and 
## a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
  	set <- function(y) {
   		x <<- y
    		m <<- NULL
  	}  
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


##  The cacheSolve function computes the inverse of the "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated 
## then cacheSolve retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
       
	 ## Return a matrix that is the inverse of 'x'

 	m <- x$getinverse()
 	if(!is.null(m)) {
 	   	message("getting cached data")
    		return(m)
 	 }
	
	data <- x$get()
  	m <- solve(data, ...)
  	x$setinverse(m)
  	m
}
