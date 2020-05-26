##Week 3 Assignment: Caching the Inverse of a Matrix


##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  set <- function(y) {
     mat<<- y 
    inverse <<- NULL
  }
  get <- function() {
    mat
  }
  setsolve <- function(solve) 
    {
    inverse <<- solve
  }
  getsolve <- function() 
    {
    inverse
  }
  list(
    set=set,
    get=get,
    setsolve=setsolve,
    getsolve=getsolve
    )

}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(mat, ...) {
  inverse <- mat$getsolve()
  if(!is.null(inverse))
    {
    message('getting cached data')
    return(inverse)
  }
  data <- mat$get()
  inverse <- solve(data,...)
  mat$setsolve(inverse)
  inverse
}
