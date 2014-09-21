## Herewith are contained two functions that together may be used to create a
## matrix and return its inverse, or retrieve it from the cache, if it's already
## been computed.

## This first function is a list of functions that can i) set the value of a matrix,
## ii) print out the matrix created in (i), iii) store the inverse of the matrix
## in the cache and iv) print out this inverse. To use it, just give it a square
## invertible matrix as input and store the result in a different variable.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function takes as input the output list of the previous function,
## checks out wether the inverse is already in the cache, in which case it retrieves
## it and gives it back, otherwise it computes the inverse with the solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
