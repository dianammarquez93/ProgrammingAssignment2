
## makeCacheMatrix is intended to store a matrix inverse, by using a specific list of functions including set and get functions that are specific to a matrix input and matrix inverse output. 
## Note: makeCacheMatrix will not inherently know how to get the inverse of the matrix, unless set/get and functions are specific to matrix input and inverse output. 
 


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Outputs stored inverses for matrix input (getting cached data), and if it doesn't have any stored matrix inverse for matrix input then it will compute and store it.  

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
