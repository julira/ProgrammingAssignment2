##The functions below are used to cahce the inverse ofa matrix
##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 

## makeCacheMatrix-function is used to create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver<-NULL
  set<-function(y){
    x<<-y
    inver<<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cachesolve function is used to compute the  inverse of the matrix generating by the function makeCacheMatrix.
##If the matrix has already inversed the function cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinverse(inver)
  inver 
}
