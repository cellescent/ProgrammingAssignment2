## makeCacheMatrix creates a matrix X, and then can store data relevant to X.
## It can be used to retrieve X or a previously cached inverse of X.
## It can also be used to set X and to cache an inverse of X.

## cacheSolve returns the inverse of the matrix stored in the list made by 
## makeCacheMatrix. It first checks if an inverse is cached, and if so, returns
## that. If it is not cached, it solves the matrix, caches it, and returns it.


## Creates a matrix X and defines functions for setting, getting, storing inverse
## of, and retrieving inverse of X.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of the matrix stored in the function list X.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
