## These pairs of functions are developed to cache the inverse of a matrix, to 
## avoid costly computation of matrix inversion. 

###############################################################################
## Example:                                                                  ##
##                                                                           ##
## test.matrix <- matrix(c(4,2,7,6), nrow=2, ncol=2)                         ##
## dim(test.matrix) <- c(4,4)                                                ##
##                                                                           ##
## x <- makeCacheMatrix(x = test.matrix)                                     ##
## inverse.matrix <- cacheSolve(x)                                           ##
##                                                                           ##
## Expected Result (inverse.matrix):                                         ##
##       [,1] [,2]                                                           ##
## [1,]  0.6 -0.7                                                            ##
## [2,] -0.2  0.4                                                            ##
##                                                                           ##
###############################################################################



## makeCacheMatrix(): Creates a special 'matrix' object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## cacheSolve(): Computes the inverse of the special 'matrix' returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the 
## cache. 

## Note: This project assumes that matrix supplied is always invertible. 

cacheSolve <- function(x, ...) {
  # Note: Input x is the output of makeCacheMatrix()
  m <- x$getinverse()
  if(!is.null(m)){
    message('getting cached data')
    return(m)
  }
  data <- x$get()
  m <- solve(data,...) %*% data # Get the inverse matrix
  x$setinverse(m)
  m
}
