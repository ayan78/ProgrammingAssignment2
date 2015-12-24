## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inversion
##get the value of the inversion
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(smat) m <<- smat
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## cacheSolve function calculates the inverse of the special "matrix" created by makeCacheMatrix
## It first checks to see if the inverse has already present in the cache. 
## If so, it retrives the cached version
## Otherwise, it calculates the matrix inverse sets the value in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
