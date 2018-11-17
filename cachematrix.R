## makeCacheMatrix function intializes x, m and assigns some default values to functions set,
## get, setinverse and get inverse. It also stores inverse matrix value in Cache until the matrix is changed

## cacheSolve returns cashed value of inverse of matrix if it exsists, when a new matrix is passed it recalculates the inverse

## MakeCache matrix intializes given matrix to an object and also assigns default values to set,
## get, setinverse and get inverse.

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


## cacheSolve returns cashed value of inverse of matrix if it exsists, when a new matrix is passed it recalculates th

cacheSolve <- function(x, ...) {
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