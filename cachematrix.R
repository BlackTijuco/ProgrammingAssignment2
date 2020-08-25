## Put comments here that give an overall description of what your
## functions do

#There are two functions
#MakeCachematrix and cachesolve, that will return the inverse of the matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL              #inverse is null in the beginning
  set <- function(y) {
    x <<- y
    inv <<- NULL
  } 
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...)
  {
  inv <- x$getInverse()
  if(!is.null(inv)) {               #this checks if it is null
    message("getting cached data")
    return(inv)               #the return value here
  }
  mat <- x$get()
  inv <- solve(mat, ...)      #calculates the inverse value
  x$setInverse(inv)
  inv              ##Returns the inverse matrix of x
}
