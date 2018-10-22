## Providing an interface for caching the inverse of a matrix.

## makeCacheMatrix acts as a container and interface for an R matrix object.
## Via the get and set fucntion a matrix object can be assigned to the container.
## In addition the container allows for the caching of an inverse of the stored matrix
## After overwriting the matrix data the cached inverse is deleted.

makeCacheMatrix <- function(x = matrix()) {
  xi <- NULL
  set <- function(y) {
    x <<- y
    xi <<- NULL
  }
  get <- function() x
  setinv <- function(inv) xi <<- inv
  getinv <- function() xi
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function returns the matrix inverse
## of the matrix that is stored in the datastructure supplied
## by makeCacheMatrix the is passed to the function.
## It will compute the inverse if no cached
## value is found, otherwise it returns the cached value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xi <- x$getinv()
  if(!is.null(xi)) {
    message("getting cached data")
    return(xi)
  }
  data <- x$get()
  xi <- solve(data, ...)
  x$setinv(xi)
  xi
}
