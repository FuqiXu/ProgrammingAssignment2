# makeCacheMatrix():create a special matrix object that can cache its reverse
# cacheSolve(): computes the inverse of the special matrix returned by makeCacheMatrix(), retieving the inverse from the cache.


## Purpose: Caching the inverse of a matrix
# Assuming the matrix is always ivertable.

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


## Write a short comment describing this function

cacheSolve <- function(x) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
