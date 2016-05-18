## The first function makeCacheMatrix will store a matrix in an environment different from current environment.
## It will store all the values from the matrix.
## Second matrix CacheSolve will directly take the stored values from makecachematrix
## and finds the inverse of it using inverse and solve function.
##The main purpose of these functions are to store the values in Cache 
## Get the values from Cache so that data is not required to entered again.

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

## 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
## the following codes will help to get a matrix and to test above two functions.

my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)