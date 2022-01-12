

#This first function (makeCacheMatrix) is a list of functions to create a
#matrix. The list contains two sets of functions, one for the matrix and the
#other to calculate the inverse of the matrix:

#set (): to define the matrix, or the inverse of the matrix
#get(): to obtain the matrix, or the inverse of the matrix

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(inverse) inv <<- inverse
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#This second function (cacheSolve) calculates the inverse of the matrix
#previously created with makeCacheMatrix. The function checks first if the
#inverse of the matrix was calculated in the first function. If that is the
#case, it gets the inverse of the matrix from the cache directly. If the inverse
#is not in the cache, it calculates it from the data and sets the value of the
#inverse of the matrix in the cache through the setsolve function.

cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}

