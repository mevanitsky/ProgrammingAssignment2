## Put comments here that give an overall description of what your
## functions do

## Creates matrix-like object that can cache inverse of matrix so it does not need to be recomputed
makeCacheMatrix <- function(x = matrix()) {
    z <- NULL
    set <- function(y){
      x <<- y
      z <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) z <<- inverse
    getinv <- function() z
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  z <- x$getinv()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinv(z)
  z
}
