## cacheMatrix.R
## Functions to calculate and cache the inverse of a matrix.

## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.
## parameter 1: an optional (square) Matrix. If NULL, default 1x1 matrix is used.

makeCacheMatrix <- function(x = matrix()) {
  theMatrix <- x #declare the original matrix and set to matrix parameter
  inverseMatrix <- NULL #declare and initialize the inverse matrix to NULL
  
  setMatrix <- function(x) {
    theMatrix <<- x #set original matrix to matrix parameter
    inverseMatrix <<- NULL #reset the inverse matrix to NULL
  }
  
  getMatrix <- function() {
    theMatrix
  } 
  
  setInverseMatrix <- function(x) {
    inverseMatrix <<- x
  }
  
  getInverseMatrix <- function() {
    inverseMatrix
  } 
  
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

## cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## parameter 1: a "Matrix" object created by makeCacheMatrix
## parameter 2 - n: Optional additional parameters to pass through to solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverseMatrix();
  if(!is.null(inverseMatrix)) {
    return(inverseMatrix)
  }
  
  theMatrix <- x$getMatrix()
  inverseMatrix <- solve(theMatrix, ...)
  x$setInverseMatrix(inverseMatrix)
  
  inverseMatrix
}
