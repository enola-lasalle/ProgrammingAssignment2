## These functions provides capabilites to store a matrix and calculate its inverse.
##  The inverse matrix being computational intensive is calculated on if has not been calculated.

## This function stores a matrix, and its inverse

makeCacheMatrix <- function(x = matrix()) {
   iMatrix <- NULL
   
   get <- function(){
      x
   }
   
   set <- function(y){
      x <<- y
      iMatrix <- NULL
   }
   
   getInverse <- function(){
      iMatrix
   }
   
   setInverse <- function(y){
      iMatrix <<- y
   }
   
   list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## This function returns the inverse of matrix
## The inverse matrix is calculated on-demand(lazily) and only if one does not exist
## If the inverseMatrix is already calculated, the existing value is provided

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        iMatrix <- x$getInverse()
        if(is.matrix(iMatrix)){
           message("returning inverseMatrix from cache")
           return (iMatrix)
        }
        iMatrix <- solve(x$get())
        x$setInverse(iMatrix)
        iMatrix
}
