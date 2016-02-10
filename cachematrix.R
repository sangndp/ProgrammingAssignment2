## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates a list of functions that can be used to get/set the input matrix and get/set the inverse of the input matrix.
makeCacheMatrix <- function(x = matrix()) { # sets x equal to an empty matrix
  m <- NULL # Set the inverse equal to NULL
  set <- function(y) { # set function assigns the argument to x
    x <<- y  # Once the set function is called, Inverse is re-set to NULL (this is important if you redefine the matrix, x)
    m <<- NULL
  }
  get <- function() x # get function returns the matrix 
  setInverse <- function(solve) m <<- solve
  # setInverse overrides the previous value of I and assigns the argument to Inverse (which is supposed to be the inverse of matrix x)
  
  getInverse <- function() m # getInverse returns the Inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  # creates a list of the functions
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the function will retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()  # Retrives the most recent value for the inverse
  if(!is.null(m)) { # If the value of Inverse is NOT null (was previously calculated), cacheSolve returns that value
    message("getting cached data")
    return(m)
  }
  
  # If the value of Inverse is NULL, then you retrive matrix x 
  # and calculate the inverse with the solve() function 
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m) # Sets Inverse to the newly calculated value
  m #Returns the new Inverse value
} 
