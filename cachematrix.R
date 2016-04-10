## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
### makeCacheMatrix function takes a matrix input and caches its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) {
    x <<- y 
    m <<- NULL 
  }
  get <- function() x 
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
### cacheSolve function takes the cache of the inverse of matrix x and solves
### for the inverse of x if not already computed
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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


# Implementation:
cacheSolve(makeCacheMatrix(matrix(c(1,2,3,5,7,6,4,8,2),3,3)))
