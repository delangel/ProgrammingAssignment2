## Function to create a matrix while caching its inverse
## If inverse has not been created, function will compute it and store it

## Create list object that caches inverse of matrix. 
## Basic syntax:
##
## flist = makeCacheMatrix()
## flist$set(x)
## fcached = flist$get()
##
## finv = cacheSolve(flist) # this computes inverse and caches it
## finv = cacheSolve(flist) # this returns cached inverse
## flist$setinv(finv)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## function to compute and cache the inverse of an object created by the makeCacheMatrix function
## basic usage:
##
## f = cacheSolve(fobj) # fobj was created by makeCacheMatrix as documented in that function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
