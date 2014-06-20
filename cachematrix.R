## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix will:
# 1. Initialize m and x.
# 2. Create functions for setting and getting the matrix




makeCacheMatrix <- function(x = matrix()) {
 	   
    cachem <- NULL
    set <- function(y) {
 		  x <<- y
 		  cachem <<- NULL
 	  }
 
 	  get <- function() x
 	  
    setinverse <- function(inversed) cachem<<-inversed
    getinverse <- function() cachem
    
    list(set = set, get = get,
      	setinverse = setinverse,
        getinverse = getinverse)
}


# cacheSolve will:
# 1. call makeCacheMatrix to calculate inverse if no cached version
# 2. Check whether cache version exists or not and return accordingly

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    print("In m...")
    print (m)
    
	            
    if(!is.null(m)) {
        message("Retrieving cached inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    
    x$setinverse(m) 
    
    m
}
