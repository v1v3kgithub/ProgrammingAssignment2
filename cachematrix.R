## Overall function discripions
## makeCacheMatrix(x) : Used to create a matrix wapper that has the ability of caching 
##  					the matrix inverse

## cacheSolve(x,...): Used to calculate the matrix inverse and cache the matrix inverse.

#This function creates a special "matrix" object that can cache its inverse.
#
# ----- Function Paramters ---------
# The function takes an optional matrix object, if this matrix object has been 
# set on calling the function then the caller need not call $set function 
# to set the underling matrix object

# ------------ Function Return ----------------
#The function returns a list of the following functions 
# (these are functin names in the list)
# set(y) : Called to set the underlying matrix (y) on the cache Matrix.
# get(): Called to get the Matrix on the cahce Matrix, 
#			returns an empty matrix if the cache matrix hastn been initialied
# setinverse(inv):  Called to set the inverse of the wrapped matrix object
# getinverse(): CAlled to get the inverse of the wrapper matrix (if avaliable else returns NULL)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Set the value for the underlying wrapped matrix 
  set <- function (y) {
    x <<- y
    # Clear the inverse of this matrix.
    inv <<- NULL
  }
  # Get the in wrapped matrix
  get <- function() x
  # Set the wrapped matrix inverse
  setinverse <- function(inverse) inv <<- inverse
  # Get the wrapped matrix inverse
  getinverse <- function() inv
  
  # Create and return the list of functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates a matrix inverse and also caches the matrix inverse.
##
## ----- Function Paramters ---------
# x: An object of type makeCacheMatrix 

## ------------ Function Return ----------------
## The inverse of the matrix (either from the caches inverse or recalcualted)
cacheSolve <- function(x, ...) {
  # Try and get the inverse from the cacheMatrix
  inv <- x$getinverse()
  
  if (!is.null(inv)) { # Cached inverse is avaliable (i.e it isnt null)
    message("getting cached data")
    # Return the cached matrix inverse
    return(inv)
  }
  # Get the underlying matrix from the cached matrix
  data <- x$get()
  # Call the solve method on the underlying matrix
  inv <- solve(data,...)
  # Save the inverse to the cached Matrix state.
  x$setinverse(inv)
  # Return the inverse.
  inv
}