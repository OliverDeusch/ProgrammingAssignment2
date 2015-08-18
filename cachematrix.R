## Coursera R Programming -- Programming Exercise 2

## Create a special matrix object that can cache its inverse.
## Create a function to calculate the inverse of the matrix stored in above object.
## If inverse is already cached and matrix not changed then simply print cached inverse.

## Usage example

## > source('~/Coursera/R_Programming/week3/ProgrammingAssignment2/cachematrix.R')
## > M <- makeCacheMatrix()     Create matrix
## > M$set(matrix(1:4, 2, 2))   Assign values
## > M$getinverse()             Can't get inverse as not calculated yet
## NULL
## > cacheSolve(M)              Calculate inverse
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > M$getinverse()             Now inverse is available
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(M)              2nd call returns cached data and does not calculate again
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5



## makeCacheMatrix() is a function to create a matrix that can cache its inverse
## The matrix will be an object with 4 functions:
## set to set (write) the matrix
## get to get (read) the matrix
## setinverse to set (write) the inverse
## getinverse to get (read) the inverse
## When the matrix is changed, the inverse is deleted

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve() is a function that calculates the inverse of a matrix
## The matrix needs to be of the special type created by makeCacheMatrix()
## If the inverse is alreay cached then it is returned
## If not the inverse is calculated using the solve() function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

