## Caching the Inverse of a Matrix:
## Pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse
## The first function in the file, makeCacheMatrix() 
## creates an R object that stores a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinvers <- function(solve) inv <<- solve
      getinvers <- function() inv
      list(set = set, get = get,
           setinvers = setinvers,
           getinvers = getinvers)
}
## The second function, cacheSolve() requires an argument that is returned by 
## makeCacheMatrix() in order to retrieve the mean from the cached value 
## that is stored in the makeCacheMatrix() object's environment.
cacheSolve <- function(x, ...) {
      inv <- x$getinvers()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinvers(inv)
      inv
      ## Return a matrix that is the inverse of 'x'
}
