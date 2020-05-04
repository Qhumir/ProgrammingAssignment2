## Put comments here that give an overall description of what your
## functions do

#This function creates an object.
#The object is a list of 3 functions defined in the main function environment.
#These 3 functions search and return objects in the cache that are in the environment of the main function.

makeCacheMatrix <- function(x = matrix()) {
  mi <- list(matrix = x, inverse = NULL)
  get <- function() mi$matrix
  setinv <- function(inv) mi$inverse <<- inv
  getinv <- function() mi$inverse
  
  list(get=get, setinv=setinv, getinv=getinv)
}


#This function looks for the inverse in the environment of the previous function.
#If the inverse is NULL, then it calculates it.
#If the inverse is not NULL, it returns the obtained value.

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

x <- matrix(rnorm(100),10,10)
solve(x)

a <- makeCacheMatrix(x)
cacheSolve(a)

# try again
cacheSolve(a)

