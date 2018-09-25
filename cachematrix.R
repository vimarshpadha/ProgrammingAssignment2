## functions for inverse matrix

## creating a matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## solving within the inverse matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrix <- x$get()
  inverse <- x$getinverse()
  if(!is.null(inverse) && identical(matrix%*%inverse,diag(nrow(matrix))) ) {
    message("getting cached inverse")
    return(inverse)
  }
  inverse <- solve(x$get())
  x$setinverse(inverse)
  inverse
}
