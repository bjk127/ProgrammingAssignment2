## The objective in this assignment is to write a pair of functions,
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## MakeCacheMatrix is a function that creates a particular matrix that can cache
## its inverse for the input 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the matrix returned by
## makeCacheMatrix. If the inverse has already been calculated and not changed,
# the cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(null)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

Test <- makeCacheMatrix(matrix(3:10, 4, 4))
print(Test)

Test$get()

