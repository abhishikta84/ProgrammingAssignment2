##  The first function creates a special "matrix" object that can cache its inverse
## 
## makeCacheMatrix function first sets the matrix and gets the value of the matrix
## Then it sets the inverse of matrix and gets the inverse

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invs <<- inverse
  getinverse <- function() invs
  list(set = set, get = get,
       seinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the matrix created above
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invs <- x$getinverse()
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data, ...)
  x$setinverse(invs)
  invs
}