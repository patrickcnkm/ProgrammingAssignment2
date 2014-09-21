## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x=matrix(),z=matrix()) {
  s1 <- NULL
  s2 <- NULL
  set <- function(y) {
    x <<- y
    z <<- y
    s1 <<- NULL
    s2 <<- NULL
  }
  get <- function() x
  getp <- function() z
  
  setsolve <- function(solve) s1 <<- solve
  
  setparam <- function(param) s2 <<- param
  
  getsolve <- function() s1
  
  s.param <- function() return(identical(s2,z))
  
  list(set = set, get = get,getp =getp,
       setsolve = setsolve,setparam = setparam,
       getsolve = getsolve,s.param = s.param)
}

## Write a short comment describing this function


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Obtain the value of last solve.
  s1 <- x$getsolve()
  
  ## Cache can be obtained under 2 conditions:
  ## 1) last solve value is not null.
  ## 2) The identity matrix of solve should be same.
  if(!is.null(s1) & x$s.param()) {
    message("getting cached data")
    return(s1)
  }
  
  ## Initialize the original matrix and identity matrix for solve.
  data <- x$get()
  spara <- x$getp()
  
  ## Work out the inverse matrix
  if (is.na(spara)) s1<-solve(data,...)
  else s1 <- solve(data,spara, ...)
  
  ## Setup the cache for both of inverse matrix and identity matrix.
  x$setsolve(s1)
  x$setparam(spara)
  
  ## Return the inverse matrix as output
  s1
  
}