## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than compute it repeatedly. 
## Below are two functions that are used to cache the inverse of a matrix.


## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1.  set the value of the vector
## 2.	get the value of the vector
## 3.	set the value of the mean
## 4.	get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets 
## the value of the inverse in the cache via the setinverse function. It is assumed that 
## the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
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


## Sample run
## > a <- matrix(c(1,2,3,4),nrow = 2, ncol = 2)
## > b <- makeCacheMatrix(a)
## > b$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## No cache in the 1st run
## > cacheSolve(b)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Retrieval of the inverse from the cache in the 2nd run
## > cacheSolve(b)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5