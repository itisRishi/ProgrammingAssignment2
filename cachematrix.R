##makeCacheMatrix function creates a list with methods to set and get a matrix and its inverse 
## cacheSolve method takes list from the above function and attempts to find inverse in the cached data, if can't then it calculates the value and sets it for future use.


## the makeCacheMatrix function will make a matrix mtx,
##and has methods to set and get values for mtx and its Inverse

makeCacheMatrix <- function(mtx = matrix()) {
  z <- NULL
     set <- function(y){
       mtx <<- y
       z <<- NULL
     }
     get <- function()mtx
     setInverse <- function(inverse) z <<- inverse
     getInverse <- function() z 
     list(set = set, get = get, 
          setInverse = setInverse, 
          getInverse = getInverse)
   
}
  




## cacheSolve method takes list from the makeCacheMatrix function and attempts to find inverse in the cached data if found the cached data is used else it calculates the value and sets it for future use.


cacheSolve <- function(mtx, ...) {
  z <- mtx$getInverse()
     if(!is.null(z)){
       message("getting cached data")
       return(z)
     }
     mat <- mtx$get()
     z <- solve(mat,...)
     mtx$setInverse(z)
     z
}
