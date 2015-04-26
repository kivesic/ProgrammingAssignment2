## The fil econtains two functions which store tha matrix and its invesre, and calculate the inverse matrix, respectively.


## The function takes a matrix an an argument and creates a list containing a function to
##
## 1.) set the value of the matrix,
## 2.) get the value of the matrix,
## 3.) set the value of the inverse matrix,
## 4.) get the value of the mean.
##
makeCacheMatrix <- function(x = matrix()) {
  
  ## Variable used to store the mean, initially set to NULL
  m <- NULL
  
  ## Function definitions
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  
  ## List of fuctions to be returned
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## The function returns the inverse matrix of the matrix x created by the previous function. It checks if the inverse has already been caclulated
## and returns is if has been stored already. Otherwise, it calculated the inverse, stores it and returns is.
cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  
  ## Try to get the stored value first
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## OK, the value has not been stored yet, get the matrix, find the inverse, store it and return it
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
  
}
