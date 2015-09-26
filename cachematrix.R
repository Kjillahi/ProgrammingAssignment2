## pair of functions that cache the inverse of a matrix by 

## This function creates a special "matrix" object that we can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL ## sets inverse to NULL if before it is computed
      set <- function(y) { ## this function sets the value of the matrix to the environment usin the <<- operator
        x <<- y
        inv <<- NULL
      }
      get <- function() x #fetches the matrix
      setinv <- function(mat_inv) inv <<- mat_inv # sets the inverse of the matrix
      getinv <- function() inv ##gets the inverse of the matrix
      list(set = set, get = get,setinv = setinv, getinv = getinv) #returns matrix,its inverse or set the matrix andits inverse
}


## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
library(MASS) ## loading the MASS Library in order to be able to use ginv()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) { ## if inverse of matrix is not NULL as set above initially then fetch catched matrix and its inverse
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- ginv(data)
    x$setinv(inv)
    inv # return inverse of matrix
}

