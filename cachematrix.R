##Gives a matrix that caches its inverse

#Function to create a cache matrix
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) invMatrix <<- inv
  getInverse <- function() invMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#Function for getting inverse
cacheSolve <- function(x, ...) {
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  data <- x$get()
  invMatrix <- solve(data, ...)
  x$setInverse(invMatrix)
  invMatrix
}
