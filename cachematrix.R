## The makeCacheMatrix function is a constructor function
## which returns four functions
## Two functions getMatrix and setMatrix get and set a matrix
## The getInverse function returns the matrix inverse from the cache
## The setInverse function caches the inverse of the matrix
## The cache here refers to the parent function , i.e. the environment
## of makeCacheMatrix  

makeCacheMatrix<- function(m=matrix())
{
  
  InverseM <- NULL ## Initialize the inverse to NULL
  
  ## The following function will be used for retrieving the original
  ## matrix
  
  getMatrix <- function()
  {
    m
  }
  
  ## The following function will set the matrix to the new value provided
  ## and set the inverse to NULL so that it is recalculated and not
  ## retrieved from the cache
  
  setMatrix <-function(y)
  {
    m <<- y
    InverseM <<- NULL
  }
  
  ## The following function will be used to retrieve the inverse matrix
  ## from the cache
  
  getInverse <- function()
  {
    InverseM
  }
  
  ## The following function will be used to cache the inverse matrix
  
  setInverse <- function(Inv)
  {
    InverseM <<- Inv
  }
  
  ## Return the contained functions as a list
  
  list(getMatrix = getMatrix, setMatrix = setMatrix, getInverse = getInverse,
       setInverse= setInverse)
  
}

## The cacheSolve function returns the inverse of the matrix
## created by makeCacheMatrix if it is cached, i.e. not NULL
## else it computes the inverse and caches it

cacheSolve <- function(x,...)
{
  
  ## Get the Inverse matrix
  
  Inverse <- x$getInverse()
  
  ## Case 1: The inverse is not NULL
  ## Return cached inverse matrix
  if(!is.null(Inverse))
  {
    message("Getting cached data")
    return (Inverse)
  }
  
  ## Case 2: The inverse is NULL and has to be recalculated
  ## The recalculated inverse matrix is cached
  
  mat <- x$getMatrix()
  Inverse <- solve(mat,...)
  x$setInverse(Inverse)
  Inverse
}
