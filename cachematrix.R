## Put comments here that give an overall description of what your
## functions do

## Writing a function to craete a special matrix object and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
      p <- NULL
    set <- function(y) {
      x <<- y
      p <<- NULL
    }
    get <- function() x
    setinv <- function(solve) p <<- solve      # calculating inverse of matrix
    getinv <- function() p                     #storing inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
  }




## this function checks if inverse of the matrix(given data) already exists or not
##IF it exists, data is retrieved and provided and if it doesn't then, inverse of matrix is calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  p <- x$getinv()                       # retrieving inverse of matrix
  if(!is.null(p)) {                   #if this value is true, result(inverse of matrix) will be provided
    message("getting cached data")
    return(p)
  }
  data <- x$get()       #if inverse(cached data) does not exist, we will calculate inverse of the matrix
  p <- solve(data, ...)
  x$setinv(p)
  p
}
