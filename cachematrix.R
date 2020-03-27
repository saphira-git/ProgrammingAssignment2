##The following functions are designed to take the inverse of a matrix, but to first check whether the inverse has already
## been calculated and stored (cached). If so, it retrieves this value rather than recalculating,  to more efficiently use 
##computer power. This is especially useful in the context of calculating the inversion of a matrix, 
##which is notoriously computationally expensive.

# makeCacheMatrix is a function which creates a special matrix
# which will be able to cache (store) its inverse - useful as this
# is computationally expensive and good to store the first time it is
#calculated

makeCacheMatrix <- function(x = matrix()) { #set a default
    m <- NULL #initialise the variable so it can be used later
    set <- function(y) { #the set function assigns a new value for 'matrix' in the parent environment
      x <<- y
      m <<- NULL
    }
    get <- function() x # return value of matrix argument
    setsolve <- function(solve) m <<- solve # assigns inv in parent environment
    getsolve <- function() m #retrieves value of inverse
    list(set = set, get = get,
         setsolve = setsolve, #allows you to use $ operator
         getsolve = getsolve)
  }


# cacheSolve checks to see whether the inverse of the matrix has already been calculated to conserve computing power. 
#It returns the cached (stored) value if it is available (of the function above). If not, it calculates the inverse 
#of the matrix.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) { #this part checks whether the inverse has already been cached, and if so, returns that instead of computing it new
    message("getting cached data")
    return(m)
  }
  data <- x$get() #otherwise, it calculates the inverse
  m <- solve(data, ...) #solve solves for the inverse
  x$setsolve(m) 
  m #returns inverse
}
