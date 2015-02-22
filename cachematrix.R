
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()){
	inverse <- NULL
    
    set <- function(y){
       x<<-y
       inverse <<-NULL
      
    }
    get <- function() m
    setInverse <- function(inv) inverse<<-inv
    getInverse <- function() inverse
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)    
  
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix  above. If the inverse has already been calculated (and the matrix has not 
## changed), then cacheSolve  retrieves the inverse from the cache.
cacheSolve <- function(m, ...){
  
    inv <- m$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)      
    }
   data <- m$get()
   inv <- solve(data)
   m$setInverse(inv)
   inv  
  
}
