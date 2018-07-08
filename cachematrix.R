## Put comments here that give an overall description of what your
## functions do
# This function will provide a more effective matrix which can cache
# the inverse value of the matrix 
## Write a short comment describing this function
#The first function, makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {
            x <<- y
            m <<- NULL
          }
          get <- function() x
          setInverse <- function(inverse) m <<- inverse
          getInverse <- function() m
          list(set = set, get = get,
               setInverse = setInverse,
               getInverse = getInverse)
}


## Write a short comment describing this function
#return the cache inverse of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
