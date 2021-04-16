## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x   #function to get the actual matrix
        setinv <- function( inverse ) inv <<- inverse
        getinv <- function() {
                              inver<-ginv(x)
                              inver%*%x }
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Changed "mean" to "solve" and "m" to "s"

cacheSolve <- function(x, ...) {
        
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
 }
