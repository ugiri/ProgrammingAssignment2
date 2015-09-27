## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
 setMatrix <- function(y) {
  
  x <<- y   # assigning matrix x in parent environment
  
  m <<- NULL 
 }
 getMatrix <- function() x
 setMatrixInverse <- function (matrixInverse) m <<- matrixInverse
 getMatrixInverse <- function() m
 list(setMatrix = setMatrix,
      getMatrix = getMatrix,
      setMatrixInverse = setMatrixInverse,
      getMatrixInverse = getMatrixInverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getMatrixInverse()
  if(!is.null(m)) {
    message("getting cached Matrix Inverse data")
    return(m)
  }
  data <- x$getMatrix()
  dataInv <- solve(data)
  x$setMatrixInverse(dataInv)
  x$getMatrixInverse()
}
