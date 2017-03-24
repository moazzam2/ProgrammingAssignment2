# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

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


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
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

# Computing the inverse of a square matrix can be done with the solve function in R. 
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# Test the cache matrix inverse functions:
#
# > x <- matrix(c(4, 1, 3, 1), nrow=2, ncol=2) 
# > x
#      [,1] [,2]
# [1,]    4    3
# [2,]    1    1[,1] [,2]
#
# > x_cache <- makeCacheMatrix(x)
# > cacheSolve(x_cache)
#      [,1] [,2]
# [1,]    1   -3
# [2,]   -1    4
#
# > solve(x)
#      [,1] [,2]
# [1,]    1   -3
# [2,]   -1    4


