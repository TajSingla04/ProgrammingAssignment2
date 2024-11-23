## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # Initialize the inverse to NULL
  
  set <- function(y) {
    x <<- y  # Set the matrix value
    inv <<- NULL  # Reset the cached inverse
  }
  
  get <- function() {
    x  # Return the matrix
  }
  
  setInverse <- function(inverse) {
    inv <<- inverse  # Cache the inverse
  }
  
  getInverse <- function() {
    inv  # Return the cached inverse
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  mat <- x$get()  # Get the matrix
  inv <- solve(mat, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the computed inverse
  
  inv
}

m <- matrix(c(4, 7, 2, 6), nrow = 2, ncol = 2)

cached_matrix <- makeCacheMatrix(m)

inv_matrix <- cacheSolve(cached_matrix)
print(inv_matrix)

inv_matrix_cached <- cacheSolve(cached_matrix)
print(inv_matrix_cached)

new_m <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
cached_matrix$set(new_m)

inv_matrix_new <- cacheSolve(cached_matrix)
print(inv_matrix_new)

