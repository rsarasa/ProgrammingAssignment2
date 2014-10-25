## These functions work along the similar lines as the examples (Caching the mean of a Vector) 
## that were provided with this assignment.   
## makeCacheMatrix: This function creates a matrix object that can cache the inverse of an input matrix. 
## cacheSolve: This function computes the inverse of the input matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

## To use these functions for their intended purpose, first run the makeCacheMatrix function,
## assigning the result to a variable that can be used as an argument to the
## second function cacheSolve. This is similar to the example functions provided  
## for the assignment.For Example:
## test <- makeCacheMatrx(). 
## When running cacheSolve, pass test as the argument: cacheSolve(test)
##More details given below each function's definition. 

makeCacheMatrix <- function(x = matrix()) {
  ## source("makeCacheMatrix.R")
  ## Run this function first. Example: test <- makeCacheMatrix()   
  ## This returns four other functions that can be used for 
  ## a) setting the input matrix: set. Before running this function you can create a 
  ##    square matrix on the console using something like mat <- matrix(rnorm(25), 5, 5), which
  ##    for example creates a 5x5 matrix. Then you can run set function on the console with the
  ##    the command: test$set(mat)
  ## b)getting the value of previously set matrix: get
  ## c)getting the value of the inverse matrix: getinv
  ## d)setting the value of the inverse matrix: setinv (this is used by the cacheSolve function
  ## while setting the value of the inverse matrix after its calculation.)
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of the matrix created using the previous function
  ## Also, x, the first argument is the assigned value of the previous function: test <- makeCacheMatrix
  ## Then use test as the argument for this function. Not necessary to pass any other arguements.
  ## source("cacheSolve.R")
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}