# These two functions allow the user to easily "cache" the inverse of a matrix
# (nesting the "solve" function within them) so that there´s no need to compute
# the inverse matrix again in case it is necessary to use it later. This is
# particularly useful when working with heavy/big matrices, since calculations
# can take a long time

## This first function called makeCacheMatrix creates an environment that has
# six elements, which last four are some more functions:
# x , matrixInv and set(), get(), setInvMatrix() and getInvMatrix().
# These elements are only found within makeCacheMatrix environment, and absent
# from the Global Environment. This is the first step on creating the cache function

makeCacheMatrix <- function(x = matrix()) {
  matrixInv <- NULL
  set <- function(y) {
    x <<- y
    matrixInv <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(solve) matrixInv <<- solve
  getInvMatrix <- function() matrixInv
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## Once an object of matrix class is passed to makeCacheMatrix, this next 
# function called cacheSolve can take an object of type makeCacheMatrix and two
# things can happen:
# 1. If no inverse matrix has been calculated from the matrix contained in
#    makeCacheMatrix using cacheSolve, cacheSolve will do it and assign it to 
#    the makeCache... environment
# 2. If cacheSolve has already been used to calculate the inverse matrix from the
#    matrix contained in makeCache... environment, it will just return the inverse
#    matrix located on it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrixInv <- x$getInvMatrix()
  if(!is.null(matrixInv)) {
    message("getting cached inverse matrix...")
    return(matrixInv)
  }
  data <- x$get()
  matrixInv <- solve(data, ...)
  x$setInvMatrix(matrixInv)
  matrixInv
}

# Here´s an example
a_matrix <- matrix(rnorm(2000^2), 2000, 2000)
myMatrix <- makeCacheMatrix(a_matrix)
# will take a while to calculate first
cacheSolve(myMatrix) 
# now it just looks for the inverse matrix already saved in cache
cacheSolve(myMatrix) 
