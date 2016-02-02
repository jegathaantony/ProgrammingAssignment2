## To find a pair of functions that cache the inverse of a matrix.
## Overall description
#   The functions allow you to create the inverse of a matrix and cache the inverse for retrieval
#   Makes use of <<- to assign variables in the parent environment
#   Output of makeCacheMatric can be passed to cacheSolve
#   Example of how to use it:
#     input <- matrix(1:9,nrow=3,ncol=3)
#     input$get()
#     input$getInverse()
#     getting cached data cacheSolve(input)
#     input$getInverse()

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
