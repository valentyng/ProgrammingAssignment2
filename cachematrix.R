## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated, then it retrieves the inverse from the cache

## create "matrix" object from matrix
makeCacheMatrix <- function(x = matrix()) {
  ## cached inversed matrix
  inv_matix <- NULL
  ## cached that matrix is not invertible 
  isnotinversible <- NULL
  ## set matrix
  set <- function(y) {
    x <<- y
    inv_matix <<- NULL
  }
  ## get matrix
  get <- function() x
  ## set inverse of the matrix
  setinverse <- function(inv) inv_matix <<- inv
  ## get inverse of the matrix
  getinverse <- function() inv_matix
  ## set/get "matrix is not invertible"
  set_invertible <- function() isnotinversible <<- TRUE
  get_invertible <- function() isnotinversible
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       set_invertible = set_invertible,
       get_invertible = get_invertible)
}


## inverse matrix or take the inverted matrix from cache
cacheSolve <- function(x, ...) {
  ## try to get cached inversed matrix
  inv_matix <- x$getinverse()
  if(!is.null(inv_matix)) {
    message("getting cached data")
    return(inv_matix)
  }
  ## try to check that matrix is not invertible
  is_invertible <- x$get_invertible()
  if(!is.null(is_invertible)) {
    message("getting cached data")
    stop("The matrix is not invertible.")
  }
  data <- x$get()
  nr_count <- nrow(data)
  ## is matrix not invertible?
  if (nr_count != ncol(data) || det(data) == 0) {
    x$set_invertible()
    stop("The matrix is not invertible.")
  }
  diagonal_matrix <- diag(1, nr_count)
  ## inverse matrix
  m <- solve(data, diagonal_matrix)
  x$setinverse(m)
  m
}
