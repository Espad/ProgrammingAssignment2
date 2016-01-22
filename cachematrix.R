## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly.
## Following two function are written to cache inverse of a matrix.

## in makeCacheMatrix we creating a list to
## 1,2 - set/get value of matrix
## 3,4 - set/get value of inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  setInverseMatrix <- function(matrix){
    m <<- matrix
  }
  getInverseMatrix <- function(){
    m
  }
  list (set = set, get = get,
        setInverseMatrix = setInverseMatrix,
        getInverseMatrix = getInverseMatrix)

}


## cacheSolve is a function that return the inverse of matrix
## if the inverse has already comptuted, than it will be returned
## if inverse is not computed, then it computes.
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        
  m <- x$getInverseMatrix()
  
  if(!is.null(m)){ 
    message("returning cached data")
    return(m)
  }
  
  message("no cached matrix was found, calculating the inverse")
  data <- x$get()
  m <- solve(data, ...)
  x$setInverseMatrix(m)
  m
  
}
