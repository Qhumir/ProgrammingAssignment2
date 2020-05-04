## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mi <- list(matrix = x, inverse = NULL)
  get <- function() mi$matrix
  setinv <- function(inv) mi$inverse <<- inv
  getinv <- function() mi$inverse
  
  list(get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv() 
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  i <- solve( x$get() , ...)
  x$setinv(i)
  message("inverse was calculated")
  i
}


# An Example

x <- matrix(rnorm(9),3,3)
solve(x)

a <- makeCacheMatrix(x)
cacheSolve(a)

# try again
cacheSolve(a)

