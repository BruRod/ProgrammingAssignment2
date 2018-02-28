## Caching the inverse of a Matrix
## We assume the Matrix is inversible


## Build a Matrix Object "cacheMatrix" (as a List) that lets you store a Matrix 
## and its inverse if it has been already calculated  

makeCacheMatrix <- function(x = matrix()) {
      inverse_x <- NULL
      set <- function(y) {
        x <<- y
        inverse_x <<- NULL
      }
      get <- function() x
      set_inverse <- function(inverse) inverse_x <<- inverse
      get_inverse <- function() inverse_x
      list(set = set, get = get,
           set_inverse = set_inverse,
           get_inverse = get_inverse)
}


## For "cacheMatrix" Objects this will calculate the inverse of the given Matrix and cache it
## if it hasnt been calculated already. If the inverse has been calculated before than 
## it will deliver the cached inverse.

cacheSolve <- function(x, ...) {
            ## Return a matrix that is the inverse of 'x'
      inverse_x <- x$get_inverse()
      if(!is.null(inverse_x)) {
        message("getting cached data")
        return(inverse_x)
      }
      orig_matrix <- x$get()
      inverse_x <- solve(orig_matrix, ...)
      x$set_inverse(inverse_x)
      inverse_x
}
