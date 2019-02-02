## As calculating the inverse of a matrix is a computationally heavy - heck, have you ever tried to 
## calculate de inverse of a 5x5 matrix? I had to, and its no fun - it is useful to cache the calculated inverse
## to avoid losing time rerunnning computations


## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The function cacheSolve computes the inverse of the special "matrix" created by 
## the previous function. Whenever the same inverse of the same matrix has been required and it 
## has been already calculated, the function will retrieve the cached inverse instead of calculating it again


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
