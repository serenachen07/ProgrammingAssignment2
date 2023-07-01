
# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setInverse <- function(inv) m <<- inv 
  
  # Function to get the inverse
  getInverse <- function() m
  
  # Return a list of functions
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

# Function to compute the inverse of the matrix and cache it
cacheSolve <- function(x, ...) {
  
  # Check if the inverse is already cached
  m <- x$getInverse()
  if (!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  
  # Compute the inverse using solve() function
  mat <- x$get()
  m <- solve(mat)
  
  # Cache the inverse
  x$setInverse(m)
  
  # Return the inverse
  m
}

#'m1' is a simple matrix 
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

#'n1' is the inverse of 'm1'
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)

#Using solve() to check that 'm1' and 'n1' are inverse of each other 
solve(m1)
solve(n1)

#A cache matrix object using the matrix 'm1'
myMatrix_object <- makeCacheMatrix(m1)

#Compute the inverse of the matrix stored in 'myMatrix_object' 
#(which should be 'n1')
#Calling cacheSolve again should retrieve it from the cache 
cacheSolve(myMatrix_object)
