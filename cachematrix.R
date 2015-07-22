## The purpose of these functions is to prevent repeated costly
## computations of inverting matrices by caching the inversions
## so that they can be recalled rather than computed again
## if the user desires to invert the same matrix

## This function creates a list of functions to be called in order to:
## 1. set the matrix
## 2. get/recall the matrix
## 3. invert the matrix
## 4. get/recall the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
cache <- NULL
set <- function(m){
  x <<- m
  cache <<- NULL
}
  
get <- function() x 
invert <- function(inverse) cache <<- inverse
getinverse <- function() cache
list(set=set, get=get, invert=invert, getinverse=getinverse)
}


## This function inverts the matrix and returns it.
## Unless the inversion of this matrix is already cached, in which
## case it recalls the cached matrix.
## If the matrix has not already been cached, it will then cache the
## matrix for future computational efficiency

cacheSolve <- function(x, ...) {
  cache <- x$getinverse()
  if(is.null(cache)){
    matrix <- x$get()
    cache <- solve(matrix)
    x$invert(cache)
    cache
  }
  message("retreiving cached matrix")
  return(cache)
        ## Return a matrix that is the inverse of 'x'
}
