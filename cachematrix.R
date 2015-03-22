## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_matix <- NULL
  set <- function(y) {
    x <<- y
    inv_matix <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inv_matix <<- inv
  getinverse <- function() inv_matix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  inv_matix <- x$getinverse()
  if(!is.null(inv_matix)) {
    message("getting cached data")
    return(inv_matix)
  }
  data <- x$get()
  diagonal_matrix <- diag(1,nrow(data))
  m <- solve(data, diagonal_matrix )
  x$setinverse(m)
  m
}
