# We call the function with a matrix, 
# compute the inverse, 
# retrieve the inverse from the cache list, 
# change the call matrix to the inverse, 
# compute the inverse
# and return the original function.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#This function computes the inverse of the special matrix returned by makeCacheMatrix. 
#If the inverse has been calculated, then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}