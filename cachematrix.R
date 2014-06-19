
## Below function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # initialize the stored inverse to NULL
  inv <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the inverse
  setinv <- function(inverse) inv <<- inverse
  # get the inverse
  getinv <- function() inv
  
  # list of all functions defined above
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Below function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # check whether the inverse has already been cached
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if above is not the case, load matrix and store it in <data>
  data <- x$get()
  # calculate the inverse using the 'solve' function
  inv <- solve(data, ...)
  # cache the inverse
  x$setinv(inv)
  
  # return the inverse
  inv
}
