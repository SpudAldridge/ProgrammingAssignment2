## This function Cache's the inverse of a matrix
## The function stores a matrix and caches its inverse

## The function creates a "matrix" object that is capable of cacheing the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
  x <<- y
  inv <<- NULL
}
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set, 
     get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}

## Computes the inverse of the "special" matrix generated
## by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'


    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached matrix")
        return(inv)
  }
  dat <- x$get()
  inv <- solve(dat, ...)
  x$setInverse(inv)
  inv
}


Spuds_matix <- makeCacheMatrix(matrix(1:9, 3, 3))
Spuds_matix$get()






