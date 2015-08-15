## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a special "matrix", which is really a list containing functions to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix
# This works in conjunction with cacheSolve. If the inverse has already been calculated, it will be retrieved through getinverse
# getinverse will return NULL if the inverse is not yet known
# if the content of the matrix is altered, the inverse is reset to NULL
makeCacheMatrix <- function(mm = matrix()) {
  # Set inverse to NULL on creation
  i <- NULL;
  set <- function(m) {
    mm <<- m
    # set inverse to NULL in in case of matrix change
    i <<- NULL
  }
  get <- function() mm
    
  # Cache inverse for future use
  setinverse <- function(si) i <<- si
  # Retrieve cached inverse (or NULL in case inverse has not been set)
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve calculated the inverse of the special "matrix" created with makeCacheMatrix(). It first checks if the inverse has 
# already been calculated. If that is the case, the computation is skipped and the inverse is returned. 
# Otherwise, the inverse is calculated and retained using the cache via the setinverse() function
cacheSolve <- function(x, ...) {
  # Check if inverse has been cached  
  i <- x$getinverse()
  # If inverse has been cached (i.e. != NULL) return cached inverse
  if (!is.null(i)) {
    return(i)
  }
  # get matrix
  data <- x$get()
  # calculated inverse
  i <- solve(data)
  # Cache inverse to prevent recalculation
  x$setinverse(i)
  i
}
