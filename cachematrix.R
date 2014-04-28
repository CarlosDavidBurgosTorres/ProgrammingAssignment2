## makeCacheMatrix: This function creates a special object "matrix" that can cache its inverse.

## makeVector: Creates a special "vector", which is really a list containing a function to:
## 1.- set the value of the vector
## 2.- get the value of the vector
## 3.- set the value of the mean
## 4.- get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
         i  <- NULL
         set  <- function(y){
                 x <<- y
                 i <<- NULL 
         }
         get  <- function() x
         setinverse  <- function(inverse) i  <<- inverse
         getinverse  <- function() i
         list(set= set, get = get, 
              setinverse = setinverse, 
              getinverse = getinverse)
 
 }

## cacheSolve: This function calculates the inverse of the "matrix" special makeCacheMatrix previously returned by.
## If the reverse is already calculated (and the matrix has not changed), then the reverse cachesolve must retrieve cache.

cacheSolve <- function(x, ...) {
         i  <- x$getinverse()
         if (!is.null(i)){
                 message("receive data in cache")
                 return(i)
         }
         data  <- x$get()
         i  <- solve(data, ...)
         x$setinverse(i)
         i
 }
rows <- 5
columns <- 5
mtx <- makeCacheMatrix(matrix(rnorm((r*c),mean=2,sd=0.2),nrow=rows,ncol=columns))
cacheSolve(mtx) 

## rnorm: this function chooses random values ​​for the matrix
