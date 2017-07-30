## These two functions cache the inverse of a matrix and store the values, to
## avoid computing value again. Given the matrix does not change, the functions
## retrieve the cached value of the inverse, rather than calculating the values
## again.

## makeCacheMatrix creates an object, x and converts it into a matrix. It also
## assigns names to each variable so they can be retrieved with the "$" symbol
## in cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(a) {
    x <<- a
    invmat <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) invmat <<- inv
  getinverse <- function() invmat
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve determines whether a previously calculated value for the inverse
## of a matrix is available. If so, it returns the cached value. Else, the
## function gets the new matrix, and calculates the inverse using the solve()
## function, and assigns this value as the new cached value.

cacheSolve <- function(x, ...) {
  invmat <- x$getinverse()
  if (!is.null(invmat)) {
    message("Getting cached value of inverse matrix")
    return (invmat)
  }
  matx <- x$get()
  invmat <- solve(matx, ...)
  x$setinverse(invmat)
  invmat
}

