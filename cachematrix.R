

## This file has two functions "makeCacheMatrix" & "cacheSolve"
## makeCacheMatrix function accepts a matrix and returns a list of functions. It aims to cache the calculated values of the inverse of a matrix
## cacheSolve function checks to see if a cached inverse is already calculated for a matrix.
## If not, it checks to see if the matrix in question in question is a square matrix and Non-singular [Non zero det]
## If both the above conditions are satisfied, it calculates the inverse of a matrix using solve() functions & sets the inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y)
		{
		x <<- y
		inverse <<- NULL
		}
	get <-function()x
	setinverse <- function (minverse)
		{
		inverse <<- minverse
		}
	getinverse<-function()inverse
	list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}




cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

 m <- x$getinverse()
        if(!is.null(m))
		{
                message("getting cached data")
                return(m)
        }
        data <- x$get()
##Check whether data is a square matrix
	  if(nrow(data) != ncol(data))
	  {
         message ("A non-square matrix was passed and it cannot have an inverse")		
         return()
	  }	
##Check whether data is a non-singular matrix i.e matrix determinant should be non zero for inverse to exist
	  if (det(data)==0)
	  {
         message ("A Singular matrix was passed and it cannot have an inverse")		
         return()
	  }	
##Use the Solve function to calculate matrix inverse
	  m <- solve(data, ...)
        x$setinverse(m)
        m

}
