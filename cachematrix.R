## Put comments here that give an overall description of what your
## functions do
## The functions help to create a matrix and it's inverse and return the value of the inverse of the matrix in the second function
## Write a short comment describing this function
## The function makeCacheMatrix creates a matrix which contains a list containing the functions to set and get the value of the matrix and it's inverse




makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(b) {
    x <<- b
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
##The function cacheSolve actually returns the inverse of the matrix that we defined in the first function 
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

