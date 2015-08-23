## The following two functions are used in pairs to cache the inverse of a matrix.
## The first function (makeCacheMatrix) creates a special "matrix" object that can cache its inverse.
## The second one (cacheSolve) either retrieve the inverse from the cache (if it has already been set)
## or it computes the inverse of the special "matrix" returned by makeCacheMatrix. 


## The function makeCacheMatrix stores a list of 4 functions:
## - set/get, which returns/changes respectively the matrix x stored in the main function;
## - setinv, which store the value of the inverse in a variable i into the main function;  
## - getinv, which return the value of the inverse stored in the variable i into the main function.
  
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


## cacheSolve takes as input the object where makeCacheMatrix is stored.
## cacheSolve verifies if the value i, stored with getinv, exists and is not NULL. 
## If it exists in memory, it simply returns a message and the value i.
## If not, data gets the matrix stored with makeCacheMatrix,
## i calculates the inverse of the vector through the solve function
## and x$setinv(i) stores it into the variable i of the makeChaceMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
