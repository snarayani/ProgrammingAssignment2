
## Creates a special matrix with function to
## which sets the inverse of itself, retrieves the inverse.
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

## Function returns the inverse of the matrix created using 
## makeCacheMatrix function. Function caches the reverse 
## of the matrix. Returns the cached copy if the input matrix is same.
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
