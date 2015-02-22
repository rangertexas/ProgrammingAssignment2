

##Below are two functions that are used to create a special object 
##that stores a matrix and cache's its inverse.



##makeCacheMatrix creates a special "Matrix", which is really a list containing a function to
## 1.set the value of the Matrix
## 2.get the value of the Matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
  
}


## Write a short comment describing this function

##The following function calculates the inverse of the special "matrix" created 
##with the above function. However, it first checks to see if the inverse has 
##already been calculated. If so, it gets the inverse from the cache and skips the 
##computation. Otherwise, it calculates the inverse of the data and sets the value 
##of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  
}
