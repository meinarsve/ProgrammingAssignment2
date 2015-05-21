## The function makeCacheMatrix makes the matrix x cacheable to be used in
## the function cacheSolve which computes the inverse of the matrix x
## Author: Martin Einarsve
## Data: 21.05.15

makeCacheMatrix <- function(x = matrix()) {
## call i.e.: y <- makeCacheMatrix(x)
## The function makeCacheMatrix makes the matrix x cacheable to be used in
## the function cacheSolve
##
## INPUT: 
## x --- A matrix populated by numeric entries
##
## OUTPUT:
## A list containing 4 functions set, get setsolve and getsolve

# Initialize m
  m <- NULL
  
# set is a function that sets x to be the value of y
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
# get is an empty function that returns x
  get <- function() {
    x
  }
  
# setsolve sets m to be the solve function in R
  setsolve <- function(solve) { 
    m <<- solve
  }
  
# getsolve is an empty function that returns m
  getsolve <- function() {
    m
  }
  
# L is the list of 4 function as the output
  L <- list(set = set, get = get,
            setsolve = setsolve,
            getsolve = getsolve)  
}



cacheSolve <- function(x, ...) {
## call: y <- cachesolve(x, ...)
##
## INPUT:  
## x --- The list that was outputted from makeCacheMatrix    
##
## OUTPUT:
## y --- The inverse of the matrix x if it is not cached 
  
# Denote the getsolve function to m
  m <- x$getsolve()
  
# if m ~= NULL get cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
# Get data from x$get
  data <- x$get()
  
# Find the inverse with the solve function
  m <- solve(data, ...)  
}

