## Matrix inversion is usually a costly computation. 
## There is some benefit to caching the inverse of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  set <- function(matr) {
    x <<- matr
    inverse <<- NULL
  }
  
  get <- function() x
  
  setSolve <- function(invr = matrix()) inverse <<- invr
  
  getSolve <- function() inverse
  
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)    
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  invr <- x$getSolve()
  if(!is.null(invr)) {
    message("getting matrix inverse from cached data")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data)
  x$setSolve(invr)
  invr  
}