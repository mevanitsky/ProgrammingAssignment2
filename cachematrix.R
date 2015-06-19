## Saves matrix as a special object that can cache inverse of matrix
## Computatesinverse of matrix unless cached version already exists
## if so, retrieves and returns cached inverse



## Creates matrix-like object that can cache inverse of matrix so it
##does not need to be recomputed 

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


## Calculates inverse of cached matrix, 
##unless inverse has already been calculated, then 
##prints message "getting cached data" and returns cached inverse

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
