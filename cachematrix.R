## Put comments here that give an overall description of what your
## functions do
## two function defined in this code are 
## 1.makeCacheMatrix to create a special matrix that cache its inverse
## 2.cacheSolve to retrive cache inverse from object


## Write a short comment describing this function
## this function return a object with three function 
## 1. set: set value of matrix
## 2. get: get value of matrix
## 3. getInverse: get inverse of matrix
## and one internal function calcInverse to calculate inverse of matrix and cache its value

makeCacheMatrix <- function(x = matrix()) {
  
  M <- x
  cacheInverse <- NULL
  
  set <- function(x) {
    M <<- x
    cacheInverse <<- NULL
  }
  get <- function() M
  
  calcInverse <- function() {
    if (is.null(cacheInverse)) cacheInverse <<- solve(M)
  }
  getInverse <- function() 
    {
      calcInverse()
      cacheInverse
    }
  
  list(set = set, get = get, getInverse = getInverse)
  
}


## Write a short comment describing this function
## this function will request inverse matrix from special object create from first function
## any calculation have been move to inside objecct

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x$getInverse()
}
