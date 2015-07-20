## This function creates a special matrix object which is
## really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. get the value of the inverse
## 4. set the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Stored inverse
  inv <- NULL

  #set method for the matrix itself
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  #get method for the matrix itself
  get <- function() x

  #set method for the inverse of the matrix
  setInverse <- function(inverseMatrix) inv <<- inverseMatrix

  #get method for the inverse of the matrix
  getInverse <- function() inv

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The function returns the inverse of a matrix.
## If this inverse has already been computed,
## it will return the stored value. If not, it will compute
## the inverse and store it for future recall.
cacheSolve <- function(x, ...) {
  #First see if we have the inverse already computed
  inv <- x$getInverse()
  if (!is.null(inv)) {
    #we have a stored value, returning it.
    message("getting cached data")
    return (inv)
  }

  #We do not have a stored value, so we must compute it.
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}

#Usage Example:
#> B = matrix(c(2,4,3,1,5,7,1,8,2), nrow=3, ncol=3)

# What do we expect?
#> solve(B)
#            [,1]        [,2]        [,3]
#[1,]  0.7301587 -0.07936508 -0.04761905
#[2,] -0.2539683 -0.01587302  0.19047619
#[3,] -0.2063492  0.17460317 -0.09523810

#>matrixB <- makeCacheMatrix(B)
#>cacheSolve(matrixB)
#[,1]        [,2]        [,3]
#[1,]  0.7301587 -0.07936508 -0.04761905
#[2,] -0.2539683 -0.01587302  0.19047619
#[3,] -0.2063492  0.17460317 -0.09523810

#>getting cached data
#cacheSolve(matrixB)
#[,1]        [,2]        [,3]
#[1,]  0.7301587 -0.07936508 -0.04761905
#[2,] -0.2539683 -0.01587302  0.19047619
#[3,] -0.2063492  0.17460317 -0.09523810