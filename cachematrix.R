## makeCacheMatrix stores supplied matrix to cache
## cacheSolve displays the inverse of a supplied matrix
## either from cache or calculates new

## Instructions:
## Create a matrix and store to an object for example:
## mynewmatrix <- matrix(rnorm(1:9),3,3)
## Make the mynewmatrix available as object in cache through
## makeCacheMatrix, example:
## cachedmatrix1 <- makeCacheMatrix(mynewmatrix)
## Invert and store inverse of cachedmatrix1 using the 
## cacheSolve function, example:
## cacheSolve(cachedmatrix1)
## This should display the inverse of the cachedmatrix1
## Executing this command a second time will draw the inverse
## from system cache

## makeCacheMatrix stores square matrix to cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve displays the inverse of a supplied matrix
## either from cache or calculates new

## Per assignment, function assumes it will be supplied a 
## square matrix, i.e. 2x2 or 3x3.

## Supplied matrixes must be in the form of an object

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
