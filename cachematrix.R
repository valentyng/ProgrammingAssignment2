## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_matix <- NULL
  isnotinversible <- NULL
  set <- function(y) {
    x <<- y
    inv_matix <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inv_matix <<- inv
  getinverse <- function() inv_matix
  set_invertable <- function() isnotinversible <<- TRUE
  get_invertable <- function() isnotinversible
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       set_invertable = set_invertable,
       get_invertable = get_invertable)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  inv_matix <- x$getinverse()
  if(!is.null(inv_matix)) {
    message("getting cached data")
    return(inv_matix)
  }
  is_invertible <- x$get_invertable()
  if(!is.null(is_invertible)) {
    message("getting cached data")
    stop("The matrix is not invertible.")
  }
  data <- x$get()
  nr_count <- nrow(data)
  if (nr_count != ncol(data) || det(data) == 0) {
    x$set_invertable()
    stop("The matrix is not invertible.")
  }
  diagonal_matrix <- diag(1, nr_count)
  m <- solve(data, diagonal_matrix)
  x$setinverse(m)
  m
}
