## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
 setMatrix <- function(y) {
  x <<- y
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
