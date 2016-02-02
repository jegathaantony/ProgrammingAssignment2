## To find a pair of functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix())
{
  inv_Matrix <- NULL
  set <- function(y) 
  {
    x <<- y
    inv_Matrix <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse)  inv_Matrix <<- inverse
  getInverse <- function() inv_Matrix
  list(set = set, get = get, setInverse = setInverse,getInverse = getInverse)
  
}

cacheSolve <- function(x, ...) 
{
  ## Inverse of 'x' will be returned
  inv_Matrix <- x$getInverse()
  if (!is.null(inv_Matrix)) {
    message("getting cached data")
    return(inv_Matrix)
  }
  ##mat creates matrix from the given set of values        
  ##input:Range of values,Row,column
  
  data <- x$get()
  inv_Matrix <- solve(data, ...)
  x$setInverse(inv_Matrix)
  inv_Matrix
}
