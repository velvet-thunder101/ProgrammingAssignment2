## Put comments here that give an overall description of what your
## functions do
## The functions first define the inverse of the matrix in the first function and return its value in the second function
## Write a short comment describing this function
## The function makeCacheMatrix is used to define the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) a <<- inverse
  getinverse <- function() a
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function
##This function actually returns the inverse of the matrix that is given
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  a <- x$getinverse()
  if (!is.null(a)) {
    message("Cached data is given as follows")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinverse(a)
  a
}

