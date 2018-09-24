## makeCacheMatrix takes a matrix object and returns a list with 4 helper functions: set() & get() for assigning
## and returning the target matrix and setInvMat and getInvMat to assign and return the inverted matrix. To access
## the matrix info is stored in the function environment.

makeCacheMatrix <- function( x = matrix()) {
  invMat <- NULL
  set <- function(y){
    x <<- y
    invMat <<- NULL
  }
  
  get <- function()x
  setInvMat <- function(inverted)invMat <<- inverted
  getInvMat <- function()invMat
  list(set = set, get = get, setInvMat = setInvMat, getInvMat = getInvMat)
}

## cacheSolve returns the inverse of an matrix object created by makeCacheMatrix. if the matrix object does not have 
## an inverted matrix stored, it will calculate an inverted matrix, if it has an inverted matrix stored it will return
## that one

cacheSolve <- function(x, ...){
  invMat <- x$getInvMat()
  if(!is.null(invMat)){
    message('getting cached data')
    return(invMat)
  }
  data <- x$get()
  invMat <- solve(data, ...)
  x$setInvMat(invMat)
  invMat
}

