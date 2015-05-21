makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { ##set allows to replace cached matrix inverse
    x <<- y
    m <<- NULL
  }
  get <- function() x ##returns matrix (non-inverse) if available
  setmatrixinv <- function(matrixinv) m <<- matrixinv ##load matrix inverse into cache
  getmatrixinv <- function() m ##return matrix inverse
  list(set = set, get = get,
       setmatrixinv = setmatrixinv,
       getmatrixinv = getmatrixinv)
}


cacheSolve <- function(x, ...) {
  m <- x$getmatrixinv() ##load matrix inverse into variable
  if(!is.null(m)) { ##if variable contains something return it and exit function
    message("getting cached data")
    return(m)
  }
  data <- x$get() ##if variable is empty, load inputted matrix
  m <- solve(data, ...) ##solve loaded matrix inverse
  x$setmatrixinv(m) ##store result into cache
  m
}
