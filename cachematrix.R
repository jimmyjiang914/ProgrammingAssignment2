## cacheSolve accepts the output of makeCacheMatrix-- which consists of a list
## of functions that are able to manipulate the state of the outputted 
## makeCacheMatrix object (the list). cacheSolve decides whether to compute
## the matrix inversion (if it sees that no cache exists), or if it returns the
## cached inverse matrix from the makeCacheMatrix list object.

## makeCacheMatrix: A function that accepts a matrix and generates a list 
##                  of functions that perform set/get operations on the original 
##                  matrix provided, as well as on the inverse of the given matrix.
##                  Caching here allows the state of the matrix and inverse matrix
##                  variables to be preserved.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) { # Set function used to update matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x # Get function used to return matrix
  setinverse <- function(inverse) i <<- inverse # Set function used to set inverse matrix as i-- inverse is computed in cacheSolve
  getinverse <- function() i # Get function used to return the inverse matrix from variable i.
  list(set = set, get = get, # Returns list of functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: A function that accepts an the output of makeCacheMatrix,
##             computes the matrix inversion if the object does not contain a
##             defined inverse matrix variable-- or returns the inverse matrix
##             if it has already been cached to makeCacheMatrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
