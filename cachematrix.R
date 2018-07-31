## Put comments here that give an overall description of what your
## functions do
##Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below it is a pair of functions that can cache the inverse of a matrix.
## Write a short comment describing this function
## this function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL 
  set <- function (y){
    x <<-y
    inver <<- NULL
  }
   get <- function()x
   setinverse<- function(inverse)inver <<- inver
   getinverse<- function()inver
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## Write a short comment describing this function
## The following function calculates the inverse of the special "matrix" created with the above function,makeCacheMatrix.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver <-x$getinverse()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  data<-x$get()
  inver <- solve(data,...)
  x$setinverse(inver)
  inver
}

