# Assignment 2
## Caching the Inverse of a Matrix


## Creates a list of functions that
## can cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}

## Computes the inverse of the matrix
## or retrieves it from cache if it is already calculated
cachesolve <- function(x, ...) { 
  i<- x$getinv()
  if(!is.null(i)) {
    message("getting cache")
    return(i)
  }
  data <- x$get()
  i<- solve(data, ...)
  x$setinv(i)
  i
  
}