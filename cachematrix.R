## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse. 


## This function is a list of function to 
## 1. set the matrix (reset mean in this case)
## 2. get the matrix 
## 3. set the inverse (passed in arg)
## 4. get the inverse 

makeCacheMatrix <- function(x = matrix()) {

       xinv <- NULL
       set <- function(y) {
                x <<- y
                xinv <<- NULL
       }
       get <- function() x
       setinv <- function(xinv1) xinv <<- xinv1
       getinv <- function() xinv
       list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function checks if the inverse exists than it gets it from the cache
## Otherwise it calculates the inverse and stores the value in the cache
## The function returns the inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

   	xinv <- x$getinv()
  
     	if(!is.null(xinv)) {
      	message("getting cached data")
            return(xinv)
      }
      data <- x$get()
	xinv <- solve(data, ...)
	x$setinv(xinv)
      xinv

}
