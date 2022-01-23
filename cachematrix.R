## Put comments here that give an overall description of what your functions do
## The functions help to create a matrix and it's inverse and return the value of the inverse of the matrix in the second function and also cache the value of the input matrix and the calculated inverse of the first matrix
## Write a short comment describing this function
## The function makeCacheMatrix creates a matrix which contains a list containing the functions to input and read the value of the matrix and it's inverse


makeCacheMatrix <- function(x = matrix()) {
  a <- NULL 
  inputmatrix <- function(b) { 
    x <<- b
    a <<- NULL
  }
  outputmatrix <- function() x
  inputinversematrix <- function(inversematrix) a <<- inversematrix 
  outputinversematrix <- function() a ##Saves the inverse of the matrix in case it needs to be printed again/ Caches the matrix
  list(inputmatrix = inputmatrix,
       outputmatrix = outputmatrix,
       inputinversematrix = inputinversematrix,
       outputinversematrix = outputinversematrix)
}


## Write a short comment describing this function
##The function cacheSolve actually returns the inverse of the matrix that we defined in the first function and also returns the cached value of the inverse matrix if the same matrix is used in calculation again
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  c <- x$outputinversematrix()
  if (!is.null(c)) {
    message("Your matrix hasn't changed so the cached data of the inverse of the previously defined matrix is printed again")
    return(c)
  }
  data <- x$outputmatrix()
  c <- solve(data, ...) ##solve function calculates the inverse of a matrix and is a predefined function in R
  x$inputinversematrix(c)
  c
}

