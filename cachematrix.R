# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly. This script is to write a pair of functions that cache the inverse of a matrix.


# makeCacheMatrix function creates a special "matrix" object that can cache its inverse 
# a list containing a function to:
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix
# is created

makeCacheMatrix <- function( x = matrix() ) {
  
  inv <- NULL
  
  # set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get the matrix
  get <- function() x
  # set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  # get the inverse of the matrix
  getinverse <- function() inv
  
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse )
  
}


# cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix
# however, it first checks if the inverse has already been computed
# if so, it retrieves the inverse from the cache and skips the computation 
# otherwise, it calculates the inverse of the matrix and sets it in the cache via the setinverse function

cacheSolve <- function( x, ... ) {
  
  # check if the inverse of the matrix is calculated - if so, get it from the cache 
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if the inverse is not yet computed, get the actual matrix ...
  data <- x$get()
  # ... calculate the inverse ...
  inv <- solve(data,...)
  # ... set it in the cache ... 
  x$setinverse(inv)
  # ... and return!
  inv
  
}
