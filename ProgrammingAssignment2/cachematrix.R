## To find inverse of a matrix passed as an argument to the function "makeCacheMatrix"
## makeCacheMatrix - get function will return a matrix that needs to be inversed. setinverse will cache the inverse of matrix to a variable.
## cacheSolve - Determine whether to calculate inverse of matrix if getinverse returns null or fetch it from cached variable.

## Returns list containing functions such as get, setinverse and getinverse. Argument will be a matrix to inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  get <- function() x
  
  setinverse <- function(mat_inv) m <<- mat_inv
  
  getinverse <- function() m
  
  list(get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns inverse of matrix. Argument will be a function "makeCacheMatrix".

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
