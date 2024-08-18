## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL # initially inverse value set to null
  set <- function(y){
    x <<- y #caching value of matrix
    I <<- NULL # set to NULL because mean will be needed to recalculated after calling set and initializing matrix
  }
  get <- function() x
  setInverse <- function(z){
    I <<- solve(x)  ##Calculates Inverse & Caches it 
  }
  getInverse <- function() I
  list(set = set, get = get, 
       setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                ## Return a matrix that is the inverse of 'x'
    #First check if Inverse is already calculated 
  m <- x$getInverse()
  if(!is.null(m)){
    print("Retrieving Cached Data")
    return(m) # return the answer
  }
  data <- x$get()
  I <- solve(data, ...) # Calculates the inverse
  x$setInverse(I) #Caches the inverse
  I
}
