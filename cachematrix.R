## makeCacheMatrix: 
## MAKECACHEMATRIX(var) takes an input of type matrix
## On first calling MAKECACHEMATRIX(var) it sets... 
## m_Inv to NULL then calls SET(var) and assigns var...
## to global variable x.
## SETINV sets m_Inv to my_mat where my_mat is...
## from CACHESOLVE(var).
## GETINV gets the inverese m_inv and everything is...
## put in a list.
## if SET(var) is called individually, it first tests...
## if the input is identical to the stored global matrix
## and if it is, leaves the matrix as it is and...
## retrieves the cached inverse. Otherwise it will reset...
## the inverse matrix to NULL to recalc the new inverse.

makeCacheMatrix <- function(x = matrix()) {
  m_Inv <- NULL
  set <- function(y) {
    if(identical(x,y)){
      message("input matrix is the same as current stored.")
      message("will re-use cached inverse.")
      return(x)
      ## return(m_Inv)
    }
    x <<- y
    m_Inv <<- NULL
  }
  get <- function() x
  setInv <- function(my_mat) m_Inv <<- my_mat
  getInv <- function() m_Inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


## cacheSolve:
## CACHESOLVE(var) takes a matrix and sets m_Inv...
## to whatever is currently in var$getInv.
## It tests if var==NULL and if TRUE, returns
## the stored inverse matrix.
## otherwise it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
  m_Inv <- x$getInv()
  if(!is.null(m_Inv)){ 
    message("getting cached data")
    return(m_Inv)
  }

  data <- x$get()
  m_Inv <- solve(data)
  x$setInv(m_Inv)
  m_Inv

}