
## This function creates a special "matrix" object that can cache its inverse.
## (works with squarred matrix with determinant different of 0)

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL  # first the invese is null
  
  # Setter and Getter ---------------------
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  #---------------------------------------
  
  # Setter and Getter of the inverse, accessible by cacheSolve
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  #----------------------------------------
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()  # Get the inverse of the makeCacheMatrix special object
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)           # If the inverse has already been calculated, return this inverse
  }
  
  data <- x$get()       # Get the content of the makeCacheMatrix special object
  
  i <- solve(data, ...) # Calcule the inverse if return NULL
  
  x$setinverse(i)       # Set the inverse in the makeCacheMatrix special object
  
  i
  
}
