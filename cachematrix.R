## The makeCacheMatrix and cacheSolve functions can work together to save time
## when needing to calculate the inverse of a matrix multiple times. 
## By using the cacheSolve function, an inverse matrix will be cached the 
## first time it is calculated (via the makeCacheMatrix function), and then can
## be read from the cache during subsequent calculations.

## makeCacheMatrix creates a pseudovector, constituting four entries.
## The four entries include variables to set and get if the function has
## previously run, and variables to set and get the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve will return the inverse of a matrix.
## If it is already cached, it will return it from the cache, otherwise it
## will calculate it, place it in the cache, and return it.

cacheSolve <- function(x, ...){
      m <- x$getsolve()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
