## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function to create, store and retrieve a matrix and its inverse to cache  
# makeCacheMatrix creates matrix type to run 4 functions
# set - stores the matrix in cache
# get - retrieves the matrix from cache
# setInverse - stores the inverse of the original matrix
# getInverse - retrieves the inverse of the original matrix

makeCacheMatrix <- function(x = matrix()){    
  m <- NULL
  set <- function(y){
    x <<- y  
    m <<- NULL #stores matrix in cache 
  }
  get <- function() x 
  setInverse <- function(solve) m<<- solve 
  getInverse <- function() m 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  # creates a list of functions
}



## Write a short comment describing this function

# cacheSolve - calculate the inverse matrix from the output of the makeCacheMatrix function
# after checking whether the calculation has been already done
# If so, it recalls the data from the cache. 
# If not,  calculates the inverse matrix then stores it in the cache

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  m <- x$getInverse()                 # query the x matrix's cache
  if(!is.null(m)){                    # see if the inverse has been already previously calculated
    message("getting cached data")    # displaying the message about getting previously calculated inverse 
    return(m)                         # return the cache  
  }
  data <- x$get()                     # get the matrix used by makeCacheMatrix function 
  m <- solve(data, ...)               # calculate the inverse of the matrix
  x$setInverse(m)                     # store the inverse matrix in cache using the makeCacheMatrix set function
}


# How to use the function
a <- matrix(1:4,2,2) # Define matrix 
a
b <- makeCacheMatrix (a) # Get the matrix
t <- cacheSolve(b)
t
