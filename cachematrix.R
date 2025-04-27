#### makeCacheMatrix & cacheSolve Functions
## Why do this? This is an example of caching


### Define makeCacheMatrix
# Function that creates a matrix object that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
# No inverse matrix calculated initially    
    i <- NULL

# Assign new matrix, y, to x  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }

# Get x from parent environment
  get <- function() x

# Updates i in parent envionment
  setinverse <- function(inverse) i <<- inverse

# Gets the cached inverse
  getinverse <- function() i
  
# Creates list of functions 
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



### Define cacheSolve
# Function that looks to cache the inverse
cacheSolve <- function(x, ...) {

# Gets cached inverse if exists
    i <- x$getinverse()

    
    if(!is.null(i)) # Checks if it exists
  {
    message("winner—getting cached data :)")
    return(i) # prints message if gets cached and doesn't run new calculation
  }
  data <- x$get() # if no cache then calculate
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


# Create a random matrix
my_matrix <- matrix(c(2, 1, 1, 2), nrow = 2, ncol = 2)

# Create cached matrix object
cachedMatrix <- makeCacheMatrix(my_matrix)

# First time solving: will calculate
cacheSolve(cachedMatrix)

# Second time solving: will use cache! Winner :) 
cacheSolve(cachedMatrix)