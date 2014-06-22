## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i  <- NULL  
  ## set the value of the matrix
  set  <- function(y){
    x <<- y
    i <<- NULL      
  }        
  ## get the value of the matrix
  get  <- function() x
  ## set the value of the inverse
  setinverse  <- function(inverse) i<<-inverse
  ## get the value of the inverse
  getinverse  <- function() i 
  ## create a list containing functions
  list(set= set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Create function to compute the inverse of the matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Check if the value has been set in the cache
  ## and return the inverse 
  i  <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  ## If inverse of the matrix haven't been cached
  ## then calculate the inverse by function solve() and cache the value
  ## Assume the matrix supplied is invertible
  data  <- x$get()
  i  <- solve(data, ...)
  x$setinverse(i)
  i      
}


## Take 2x2 invertible matrix a as an example
## No inverse in the cache
a<-matrix(c(4,2,7,6),2,2)     ## create matrix a
x<-makeCacheMatrix(a)         ## set matrix value a 
y<-cacheSolve(x)              ## calculate inverse and set in to the cache
y                             ## matrix y, the inverse of matrix a
round(y%*%a)                  ## check if y is the inverse of a
x$getinverse()                ## check if the inverse is set to the cache
## Inverse in the cache
y<-cacheSolve(x)              ## return inverse since the inverse is cached 
y                             
round(y%*%a)                  ## check if y is the inverse of a



