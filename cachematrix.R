## This pair of functions serve as a wrapper on storing
## and calculating the inverse of a matrix. The advantage
## here is that the inverse of the matrix is cached after
## being computed once, in case the inverse is requested again
## 
## An example of usage: 
## m <- matrix(rnorm(100, 0, 10), nrow=10, ncol=10)     # creates a random 10x10 matrix
## mMatrix <- makeCacheMatrix(m)                        # stores the matrix with our function
## mMatrixInv <- cacheSolve(mMatrix)                    # computes the inverse of the matrix
## mMatrixInv <- cacheSolve(mMatrix)                    # draws the inverse from cache without re-computing

## stores the parameter matrix in a custom object
## that includes four functions, to get the matrix, set the matrix, 
## get the inverse of the matrix, and set the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## takes as a parameter an object created by makeCacheMatrix
## cacheSolve returns the inverse of the matrix, delivering
## the cached version if it has been previously computed, and
## caching the inverse if it had not been already computed
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
