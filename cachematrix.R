## How to test it:
#> m <-matrix(c(3, 64, 42, 71), 2, 2)
#> x <- makeCacheMatrix(m)
#> cacheSolve(x)

## Check result:
#> cacheSolve(x) %*% m 


## Matrix Class with get/set properties cached the result
## and avoid the cost of redundant caculations 
makeCacheMatrix <- function(x = matrix()) {
  #saves original matrix
  i <- NULL
  
  #get/set original matrix
  set <- function(x_) {
    x <<- x_
    i <<- NULL
  }
  get <- function() x
  #get/set matrix' inverse cached result
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Cache 
cacheSolve <- function(x, ...) {
  ## Return the matrix cached value of the the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  #Cache is empty, lets calculate the inverse...
  data <- x$get()
  i <- solve(data, ...)
  #Save the result in the cache
  x$setInverse(i)
  i
}
