## Cache Example - Matrix inversion
## Creating cache for Materi Inverse

makeCacheMatrix <- function(x = matrix()) {
  
  x_inverse <- NULL
  set <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) x_inverse <<- inverse
  get_inverse <- function() x_inverse
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}


## Function to return matrix inverse from cache if it exist
## if not available calculate using cachesove()

cachesolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  inverse
}
