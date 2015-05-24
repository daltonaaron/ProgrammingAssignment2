##1.makeCacheMatrix: This function creates a special "matrix" object that can ##cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
  s <- NULL 
  set <- function(y) { 
    x <<- y 
    s <<- NULL} 
  get <- function() x 
  setmatrix <- function(solve) s <<- solve 
  getmatrix <- function() s 
  list(set = set, get = get, 
       setmatrix = setmatrix, 
       getmatrix = getmatrix) }

##2.cacheSolve: This function computes the inverse of the special "matrix" 
cachematrix <- function(x, ...) { 
  s <- x$getmatrix() 
  if(!is.null(s)) {  message("getting cached matrix") 
  return(s) } 
   data <- x$get()  
   s <- solve(matrix, ...)  
   x$setmatrix(s) }


