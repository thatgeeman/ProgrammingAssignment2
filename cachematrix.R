## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object 
# that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inversematrix <<- inv # set the newly computed inverse in cache
  getinverse <- function() inversematrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) # return the list of objects
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversematrix <- x$getinverse() # first attempt to check if in cache
  if(!is.null(inversematrix)) { # if yes proceed
    message("getting cached data")
    return(inversematrix)
  }
  data <- x$get()
  inversematrix <- solve(data, ...)
  x$setinverse(inversematrix)
  inversematrix
}
