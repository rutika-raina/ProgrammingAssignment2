#Catching the inverse of a matrix
#Writing two functions to first cache the inverse of a special matrix and then compute its inverse
#or retrieve the inverse from the cache.

#Creating a function for a special matrix object that can cache its inverse
makeCacheMatrix <- function(x= matrix()) {  
       r <- NULL
     set <- function(y) {
         x <<- y
         r <<- NULL     
     }
     get <- function() x
     setinverse <- function(inverse) r <<- inverse
     getinverse <- function() r
     list(set= set, 
          get= get,
          setinverse = setinverse,
          getinverse = getinverse)     
     }

#Creating a function to compute the inverse of the special matrix returned by makecachematrix above
cacheSolve <- function(x, ...) {
   r <- x$getinverse()
   if (!is.null(r)) {
      message("getting cached data")
      return(r)     }   
   data <- x$get()
   r <- solve(data, ...)
   x$setinverse(r)
   r
}
