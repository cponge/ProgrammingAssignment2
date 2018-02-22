##  This pair of functions can caluclate the inverse of a matrix.  If you end up needing to calculate the same inverse a second time, the answer can be pulled from
##The cache instead of spending computer time recalculating.


## Sets up the matrix soo we can tell if calculations are done on it and easily save the answers.
makeCacheMatrix <- function(x = matrix()) {
  ###left text as "mean" instead of matrix for simplicity's sake
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## checks to see if matrix inverse has been done before.  If so, grab that info from cache.  if not, solve and cache,
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
        
}


