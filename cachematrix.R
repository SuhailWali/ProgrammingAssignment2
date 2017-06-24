## This program is part of Week3 - Programming Assignment2 of R course on Coursera.org
## Author : Suhail Wali
## The program consists of two functions:
##
## 1. makeCacheMatrix : This function creates a list whose elements are actually functions to perform the following
##    tasks:
##    a. create function to set the value of the matrix
##    b. create function to get the value of the matrix  
##    c. create function to set the inverse of the matrix  
##    d. create function to get the inverse of the matrix
##
## 2. cacheSolve : This function accepts the makeCacheMatrix as an argument and then checks if the inverse has been 
##    computed already. If, so it gets the inverse from the cache. Otherwise, it computes the inverse of the matrix
##    
##

# Accept a matrix as an input and create a list with functions as its elements. 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

## Example function call
## cacheSolve(makeCacheMatrix(matrix(c(2,4,6,8),2,2)))
