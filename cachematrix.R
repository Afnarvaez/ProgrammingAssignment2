## returns a list containing functions to:
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

makeCacheMatrix<- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## returns the inverse of the original matrix passed to makeCacheMatrix()

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # if the inverse has already been calculated
  # gets it from the cache and skips the computation. 
  if (!is.null(inv)){
   
    message("getting cached data")
    return(inv)
  }
  
  mat.data = x$get()
  inv <- solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)     
}
