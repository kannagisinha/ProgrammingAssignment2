## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##set- method to set the matrix and it's inverse as NULL
##get - returns the matrix, setInverse - method to set inverse of matrix
##getInverse - method to get inverse of matrix 
makeCacheMatrix <- function(q = matrix())
{
  
  ## Initialize the inverse property
  p <- NULL
  
  ## Method to set the matrix
  set <- function( matrix ) 
  {
    q <<- matrix
    p <<- NULL
  }
  
  
  get <- function() 
  {
    ## Return the matrix
    q
  }
  setInverse <- function(inverse) 
  {
    p <<- inverse
  }
  
  
  getInverse <- function() 
  {
    
    p
  }
  
  ## Return a list of the methods
  return(list(set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse))
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(inv_matrix) ) {
    message("getting cached data")
    return(inv_matrix)
  }
  else
  {
    ## Get the matrix from our object
    d <- x$get()
    
    ## Calculate the inverse using matrix multiplication
    inv_matrix<- solve(d) %*% d
    
    ## Set the inverse to the object
    x$setInverse(inv_matrix)
    
    ## Return the matrix
    inv_matrix
  }
}

