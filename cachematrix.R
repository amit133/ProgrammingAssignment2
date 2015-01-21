## A pair of functions that cache inverse of a matrix

## makeCacheMatrix() function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # m stores the inverted matrix, initialized to NULL
  m <- NULL
  err <- NULL
  
  # Function to set the matrix whose inverse has to be calculated
  setData <- function(y) {
    x <<- y
    m <<- NULL
  }

  # Function to get the matrix whose inverse is calculated
  getData <- function() x

  # Function to cache the error encounterd during inverse operation
  setError <- function(y) err <<- y
  
  # Function to get the cached error encounterd during inverse operation
  getError <- function() err
  
  # Function to set the value of inverse of a matrix stored in x
  setInv <- function(invertedMatrix) {
    m <<- invertedMatrix
  }
  
  # Function to fetch the catched inverse of the matrix x
  getInv <- function() m

  # This list is a collection of all the functions offered by custom matrix
  list(setData = setData, getData = getData,
       setInv = setInv, getInv = getInv,
       setError = setError, getError = getError)
}


## cacheSolve() function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  # Try to fetch the cached inverse
  m <- x$getInv()
  
  # check if it was first run
  if( !is.null(m)) {
    message("Getting cached data...")
    return(m)
  } else if(!is.null(x$getError())) { # If Error was set in previous attemp
    message("Returning cached Error encountered in inverse operation...")
    return (x$getError())
  }
  
  # Get the input matrix
  mat <- x$getData()
  
  # Inverse the matrix
  invMat <- tryCatch(solve(mat), error = function(e) e)
  
  if(any(class(invMat) == "error")) {
    message("Error encountered in inverse operation...")
    x$setError(invMat)
    return(invMat)
  }
  
  # Cache the value of inverted matrix
  x$setInv(invMat)
  
  return(invMat)
}
