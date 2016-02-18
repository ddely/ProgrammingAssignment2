## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      minv <- NULL
      set <- function(y) {
            x <<- y
            minv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) minv <<- inverse
      getinv <- function() minv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinv()
        if(!is.null(s)) {
                message("Getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinv(s)
        s
}
        
        
