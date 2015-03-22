## Below are two functions that are used to create a special object
## that stores a matrix and cache's its inverse

## This first function creats a special "matrix" object that can cache its 
## inverse. It contains a list of 4 functions to 
##  1) set the value of the matrix
##  2) get the value of the matrix
##  3) set the value of the inverse
##  4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This following function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. It first checks to see if the inverse 
## has already been calculated. If so and the matrix has not changed, it 
## retrieves the inverse from the cache. Otherwise, it calculates the inverse 
## of the matrix and sets the value of the inverse in the cache via the 
## setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
