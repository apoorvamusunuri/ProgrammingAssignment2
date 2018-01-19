## I have 2 function to find the inverse of a matrix, using a value storedin cache if available.


## This is the first function. It sets and gets elements used by the second function.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(matrix_inverse) inv <<- matrix_inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The second function takes the list created by the first function as an input to find the inverse 
## of a given matrix. If inverse already exists in cache then that is used instead of recomputing.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

##the end