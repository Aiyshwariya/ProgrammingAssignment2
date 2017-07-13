##Both functions together are used to compute the inverse of the matrix
## makeCacheMatrix: creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    ##sets the value of the matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x
  ##gets the value of the matrix
  setinverse <- function(invers) i <<- invers
  ##sets the value of the inverse matrix
  getinverse <- function() i
  ##gets the value of the inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: computes the inverse of the special "matrix". If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  #the if statement makes sure the inverse isn't computed again
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  #computation and caching the inverse matrix
  i
}
