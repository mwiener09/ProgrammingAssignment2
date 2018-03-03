## The following two functions compute the inverse of an invertible matrix

## Creates a special matrix object that passes matrix an can cache its inverse
  # set the value of the matrix
  # get the value of the matrix
  # set the value of the inverse
  # get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv
  
  return(list(set = set,  get = get
              ,setinverse = setinverse
              ,getinverse = getinverse))
}


## Computes the inverse of the object returned from makeCacheMatrix above.
## if the inverse has already been calculated, cacheSolve will returned 
## the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  invertible.matrix <- x$get()
  inv <- solve(invertible.matrix, ...)
  x$setinverse(inv)
  inv
}


#### to check uncomment the block of code below 
# # function to create easily invertible matrix (from R documentation)
# hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") } 
# # create invertible matrix; change input to hilbert if desired
# h <- hilbert(7); h
# object <- makeCacheMatrix(h)
# # the following two outputs should be identical
# cacheSolve(object)
# solve(h)