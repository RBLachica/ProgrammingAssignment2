##Function that stores a matrix and cache's its inverse.
##1)set the value of matrix (mset)
##2)get the value of matrix (mget)
##3)set the value of inverse (iset)
##4)get the value of inverse (iget)

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL 						##Assign 'NULL' to inverse
	mset <- function(y) {			
		x <<- y 						##Set matrix 'x'
		inverse <<- NULL
	}
	mget <- function() x 					        ##Return matrix 'x'
	iset <- function(solve) inverse <<- solve 	                ##Cache the value of the inverse 
	iget <- function() inverse 			        	##Return inverse
	list(mset= mset, mget = mget,
	     iset = iset,
	     iget = iget)
}


##Function that calculates the inverse of matrix from the above function
cacheSolve <- function(x, ...) {			                ##Return matrix that is the inverse of 'x'
	inverse <- x$iget()				                ##Get inverse
	if(!is.null(inverse)) {				                ##Check for the presence of inverse
		message("getting cached data")	#Display message
		return(inverse)
	}
	data <- x$mget()					        ##Get the atrix
	inverse <- solve(data, ...)			                ##Use solve() to compute inverse
	x$iset(inverse)					                ##Cache the inverse
	inverse 						        ##Return the inverse
}

