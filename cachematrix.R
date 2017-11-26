## cachematrix.R
## Pair of functions to allow the inverse of a matrix to be cached, speeding repeated solution.

## makeCacheMatrix: create a special 'matrix' list that allows the results to be cached.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    ## Set a new matrix, and clear the cache.
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Calculate the inverse of a CacheMatrix object. If it has previously been cached, return this instead.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Retrieving cached data.")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


## Test execution of system.
test_execution <- function() {
  test_matrix <- matrix(c(1,2,3,4,5,6,7,8,8), nrow=3, ncol=3)
  x <- makeCacheMatrix(test_matrix)
  message("makeCacheMatrix inverse matches actual inverse:")
  message(all(cacheSolve(x) == solve(test_matrix)))
  message("makeCacheMatrix caches and still matches actual inverse:")
  message(all(cacheSolve(x) == solve(test_matrix)))
}
