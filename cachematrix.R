## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## It creates a special array so that "cacheSolve" function can find the inverse of this array.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## It calculates the inverse of the array created by "makeCacheMatrix" function. If the array has been computed, return it directly.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  result <- x$get()
  inv <- solve(result, ...)
  x$setinv(inv)
  inv
}
