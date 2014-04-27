## This file contains 2 functions makeCacheMatrix and cacheSolve
## The functions are used to cache the inverse of an input matrix and
## return the inverse of the matrix from the cache first and compute it
## if not present in the cache

## To execute, after sourcing the file, make a matrix first
## a <- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(a)
## cacheSolve(a) again to verify result is being retrieved from cache

## This function is used to store the data for matrix and it's inverse
## It defines 4 functions, get(), set(), getinverse() and setinverse()
## to retrieve or store the date for matrix and it's inverse respectively

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(z) m <<- z
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function checks for the inverse of the matrix in cache created by
## makeCacheMatrix() first. If found, it retuns the result from the cache.
## Otherwise, it computes the inverse and stores it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting data from cache")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
