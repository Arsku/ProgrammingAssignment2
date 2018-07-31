## Programming Assignment 2 for R Programming on Coursera
## Caching the Inverse of a Matrix


## makeCacheMatrix generates an object, that has a matrix, if computed its 
## inverse as well a functions to set and get the matrix or inverse

makeCacheMatrix <- function(x = matrix()) { 
  i <- NULL
  set <- function(y, ...) {
    x <<- matrix(y, ...)
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## checks an object made with makeCacheMatrix if there is an inverse already
## if not it generates it, if yes, it gets it from the object

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
