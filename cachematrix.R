## Functions to create and handle special matrices that can cache
## their own inverse.
##
## USAGE example:
##
## originalMatrix <- matrix(1:4, 2, 2, byrow = TRUE)
## cacheableMatrix <- makeCacheMatrix(originalMatrix)
## cacheSolve(cacheableMatrix)
##
##
## The second time you run "cacheSolve(cacheableMatrix)"
## the inverse will be fetched from the cache. A message
## will confirm that this is happening.

makeCacheMatrix <- function(matrix = matrix()) {
  ## Creates a special object that can cache the inverse of
  ## the matrix passed as argument. The matrix passed as a parameter
  ## must be invertible.
  ##
  ## Available functions of the object:
  ##
  ## $get(): gets the original matrix
  ## $set(): replaces the content with a new matrix.
  ##         This will reset the pre-cached inverse (if any).
  ## $setInverse(): sets the cached inverse.
  ## $getInverse(): gets the cached inverse.
  inverse <- NULL

  set <- function(new_matrix) {
    matrix <<- new_matrix
    inverse <<- NULL
  }

  get <- function() {
    matrix
  }

  setInverse <- function(new_inverse) {
    inverse <<- new_inverse
  }

  getInverse <- function() {
    inverse
  }

  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse
       )
}

cacheSolve <- function(cacheableMatrix, ...) {
  ## Calculates, caches and then prints the inverse of the matrix
  ## stored in the special cacheable matrix object that needs to be
  ## created previously using the makeCacheMatrix function.
  ## (See USAGE at the beginning of the file).
  inverse <- cacheableMatrix$getInverse()

  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }

  data <- cacheableMatrix$get()
  inverse <- solve(data, ...)
  cacheableMatrix$setInverse(inverse)
  inverse
}
