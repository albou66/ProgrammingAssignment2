## Create a cache matrix from a generic matrix

makeCacheMatrix <- function(x = matrix()) {
  ## initialize the inverse matrix to NULL
  im <- NULL
  ## Define the set function
  set <- function(y) {
    ## save the original matrix
    x <<- y
    ## reinitialize the inverse matrix
    im <<- NULL
  }
  ## Define the get function that return the original matrix
  get <- function() x
  ## Define the setinverse function that save the inverse matrix
  setinverse <- function(m) im <<- m
  ## Define the setinverse function that save the inverse matrix
  getinverse <- function() im
  ## return the list of functions of a cache matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Return a matrix that is the inverse of 'x'
## 'x' is a cache matrix created by function makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getinverse()
    if(!is.null(im)) {
      message("getting cached data")
      return(im)
    }
    ## get the matrix element
    data <- x$get()
    ## perform the inversion
    im <- solve(data, ...)
    ## save the inverted matrix in the cacheMatrix
    x$setinverse(im)
    ## return the inverse matrix
    im
}
