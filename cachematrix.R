## At first, we create an object containing functions for getting and setting
## the matrix and inverse of the matrix
## The second function calculates the inverse of the matrix
## The inverse is cached for later use

## The output of the function is a list containing 4 functions
## Function set put a value of y to the variable x
## Variable x is defined in upper environment in the def of makeCacheMatrix
## Function get returns value of matrix x
## Function setinverse set an inverse matrix m
## Function getinverse returns value of inversed matrix m

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Direct output of the function is inversed matrix m
## Function takes input x which is a list of functions on matrix defined above
## If the inverse matrix already exists the return value is existing matrix
## Otherwise we put a matrix to solve into variable data
## Then we get the inverse matrix m using function solve
## Matrix m is put into x and returned to the console

cacheSolve <- function(x, ...) 
{
  m <- x$getinverse()
  if(!is.null(m))
  {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
