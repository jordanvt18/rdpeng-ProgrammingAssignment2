# Programming Assignment 2 Lexical Scoping
# Create a special object that stores a matrix and caches its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  } 
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


# cache the inverse value
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

# Demo
matriz1 <- matrix(c(1:4), 2, 2)
matriz1

matriz2 <- matrix(c(5:8), 2, 2)
matriz2

matriz3 <- matrix(c(8:11), 2, 2)
matriz3

matriz1 %*% matriz3

matriz3 %*% matriz1

solve(matriz1)

solve(matriz3)

myMatrix_object <- makeCacheMatrix(matriz1)

# Debe retornar exactamente mi matriz3
cacheSolve(myMatrix_object)

# llamando cacheSolve otra vez
cacheSolve(myMatrix_object)

