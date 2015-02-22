#get inverse of a matrix. get cached inverse if its already calculted and 
#matrix has not changed 

#makeCacheMatrix function creates a special "matrix" object which can 
#set the value of matrix. get the value of matrix.
#cache inverse. get cached inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setmatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(setmatrix = setmatrix, 
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)  
}

#cacheSolve function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve will retrieve the 
#inverse from the cache.
cacheSolve <- function(x) {
  #get cached inverse
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$getmatrix()
  
  #calculate inverse
  i <- solve(data)
  x$setinverse(i)
  i
}
