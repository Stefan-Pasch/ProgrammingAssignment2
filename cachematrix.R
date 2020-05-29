## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  h <- NULL         ## initializing
  set <- function(y){  ## define set function
    x <<- y     ## value in parent environment
    h <<- NULL  ##initializing in parent environment
  }
  get <- function()x  
  setInverse <- function(inverse) h <<- inverse  ## assign value of inverse in parent environment
  getInverse <- function() h 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of the matrix
  j <- x$getInverse()
  if(!is.null(j)){
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}

### test with small example
mat <- matrix(c(2, 4, 3, 1), nrow = 2, ncol = 2)
z<- makeCacheMatrix(mat) 
cacheSolve(makeCacheMatrix(mat))  ## viola ;)