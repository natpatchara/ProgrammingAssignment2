## Put comments here that give an overall description of what your
## functions do
## two function defined in this code are 
## 1.makeCacheMatrix to create a special matrix that cache its inverse
## 2.cacheSolve to retrive cache inverse from object


## Write a short comment describing this function
## this function return a object with three function 
## 1. set: set value of matrix
## 2. get: get value of matrix
## 3. setInverse: set inverse of matrix
## 4. getInverse: get inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  
  M <- x
  cacheInverse <- NULL
  
  set <- function(x) {
    M <<- x
    cacheInverse <<- NULL
  }
  get <- function() M
  
  setInverse <- function(inverse) {
    cacheInverse <<- inverse
  }
  getInverse <- function() cacheInverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## Write a short comment describing this function
## this function will request inverse matrix from special object create from first function
## it will check for any cached inverse if there exist it will return cached value 
## else it will calculate new inverse and store it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("load cached data")
    return(inverse)
  }
  
  mat <- x$get()
  
  inverse <- solve(mat, ...)
  x$setInverse(inverse)
  inverse
}
