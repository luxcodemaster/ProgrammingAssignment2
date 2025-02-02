## makeCacheMatrix function takes a matrix as an argument, stores it and returns a list of functions to apply on it.
## x$getinverse will get the inverse matrix data
## x$setinverse will set the inverse matrix
## x$get will get the matrix data
## x#set will set the matrix data and reset the inverse to NULL

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve has one argument of type makeCacheMatrix. The three functions of makeCacheMatrix x used are:
## x$getinverse will get the inverse matrix data from makeCacheMatrix
## x$setinverse will pass the inverse matrix from makeCacheMatrix
## x$get will get the matrix data
##
## The function will check if the inverse data is present in the passed object. 
## If not, it computes the inverse and then sets the result in the makeCacheMatrix object.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
