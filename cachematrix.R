makeMat <- function(m = matrix()) {
  inv <- NULL
  
  setM <- function(y) {
    m <<- y
    inv <<- NULL
  }
  
  getM <- function() {
    m
  }
  
  setInv <- function(i) {
    inv <<- i
  }
  
  getInv <- function() {
    inv
  }
  
  list(setM = setM, getM = getM, setInv = setInv, getInv = getInv)
}

getInvMat <- function(x, ...) {
  inv <- x$getInv()
  
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  m <- x$getM()
  inv <- solve(m, ...)
  x$setInv(inv)
  
  inv
}
m <- matrix(c(4, 7, 2, 6), 2, 2)
cache <- makeMat(m)

inv1 <- getInvMat(cache)
print(inv1)

inv2 <- getInvMat(cache)
print(inv2)

m2 <- matrix(c(1, 2, 3, 4), 2, 2)
cache$setM(m2)

inv3 <- getInvMat(cache)
print(inv3)
