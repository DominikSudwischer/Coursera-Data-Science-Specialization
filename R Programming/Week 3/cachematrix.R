## These functions create a special kind of matrix for a cached version of computing the inverse matrix.

## The function below creates a function to make a special kind of matrix which is able to to cached operations.
## It comprises a list with function to get and set the values of the matrix and its inverse matrix. Further
## information can be found within the code.

makeCacheMatrix <- function(x = matrix())
{
  ## m is the stored inverse matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## 'get' just returns the underlying data
  get <- function() x
  ## 'setinv' stores the cashed value in the variable m. The outer workspace is accessed using the <<- operator
  setinv <- function(inv) m <<- inv
  ## 'getinv' returns the cashed value
  getinv <- function() m
  ## the return value is a list of those 4 functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function calculates the the inverse matrix of a cache matrix created with the makeCacheMatrix function.
## If the inverse matrix has already been computed, it will take the cached matrix instead of re-computing it.
## More information can be found within the code.

cacheSolve <- function(x, ...)
{
  ## First, we look at the stored value for the inverse which might be NULL.
  m <- x$getinv()
  if(!is.null(m)) {
    ## If m is not NULL, it has been computed before and is stored in the cache. So it can be returned immediately.
    message("getting cached data")
    return(m)
  }
  ## If not, we first get the underlying data (the raw matrix) ...
  data <- x$get()
  ## ... and call 'solve' to compute the inverse matrix.
  m <- solve(data, ...)
  ## The computed matrix is stored via the 'setinv' function defined above.
  x$setinv(m)
  ## m is returned after the computation.
  m
}
