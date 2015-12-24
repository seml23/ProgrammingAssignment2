## Put comments here that give an overall description of what your
## functions do

#####These functions create a special cache matrix to store the elements of the inverse matrix
#####computed.  When the cacheSolve function is called, if the inverse matrix has previously
#####been calculated, it will pull the matrix from the cache.  Otherwise, it will compute the 
#####inverse matrix and store that to the cache.

## Write a short comment describing this function

#####The makeCacheMatrix creates a special object of the "matrix" class so that an inverse
#####matrix can be stored.  This special matrix object is called "mtx".  The makeCacheMatrix
#####function is an outer function consisting of inner functions that:  set the value of the 
#####mtx matrix, get the value of the mtx matrix, set the value of the inverse matrix, and get
#####the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  mtx <- NULL
  set <- function(y) {
    x <<- y
    mtx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mtx <<- inverse
  getinverse <- function() mtx
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

#####The cacheSolve will return the value of the inverse matrix.  If the inverse matrix was
#####previously computed, then it will skip the computation and return the inverse matrix 
#####from the cache and print the message "Getting cached inverse matrix".  Otherwise, it
#####will compute the inverse matrix using the solve() function, and will set the value of the
#####inverse matrix via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  mtx <- x$getinverse()
  if(!is.null(mtx)){
    message("Getting cached inverse matrix")
    return(mtx)
  } else {
    mtx <- solve(x$get())
    x$setinverse(mtx)
    return(mtx)
  }
}
