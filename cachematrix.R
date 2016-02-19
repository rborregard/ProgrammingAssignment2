##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.




makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y=matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(invVal) {
    m <<- invVal
    return(m)
  }   
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) 
  {
  n <- x$getInverse()
  
  if(!is.null(m)) && is.matrix(n){
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  
  n <- trysolve(data, ...)
    solve(data)
  x$setInverse(n)
  n
}

