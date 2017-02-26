## A pair of functions that cache the inverse of a matrix.
## expected output of a inversed 2x2 Matrix: swapped positions of a and d, negatives in front of b and c, and everything divided by the determinant (ad-bc).
## code based on Coursera course example on vector caching
## the 'solve' function in R used in this code accepts only square matrices, other matrices will give error

#first function 'makeCacheMatrix'returns a list containing functions to
##1.set the matrix
##2. get the matrix
##3. set the inverse
##4. get the inverse
##this list is input to 2nd function 'cacheSolve()'

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
} 


## second function: return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("gettting cached matrix data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv      
}

##testing the code
a<-matrix(c(2,3,4,8),nrow=2,ncol=2)
a
a1<-makeCacheMatrix(a)
cacheSolve(a1)
c<-matrix(c(1,3,2,5,1,2,7,2,2,1,4,2,5,7,1,0), nrow=4,ncol=4)
c1 <- makeCacheMatrix(c)
cacheSolve(c1)
