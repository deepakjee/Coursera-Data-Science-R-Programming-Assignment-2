# ############################################################
#
# Example Usage / Output
#
#	> mx<-matrix(c(4,3,2,1),nrow=2)
#	> cx<-makeCacheMatrix(mx)
#	> cx$getMatrix()
#	     [,1] [,2]
#	     [1,]    4    2
#	     [2,]    3    1
#	
#	> cacheSolve(cx)
#		     [,1] [,2]
#		[1,] -0.5    1
#		[2,]  1.5   -2
#	> cacheSolve(cx)
#	getting cached data
#		     [,1] [,2]
#		[1,] -0.5    1
#		[2,]  1.5   -2
#
# ############################################################
#
# makeCacheMatrix
#	function to return a list of functions to;
#	setMatrix - to set the value of a matrix
#	getMatrix - to get the value of a matrix
#	setInverse - to set the value of the matrix inverse
#	getInverse - to get the value of the matrix inverse
#
makeCacheMatrix <- function(x = matrix()) {

	# "i" is the cached inversed matrix
	i<-NULL

	# set the matrix
	setMatrix<-function(y) {
		x <<-y
		i <<-NULL
	}

	# get the matrix
	getMatrix<-function() {
		x
	}

	# set the inverse matrix
	setInverse<-function(inverse) {
		i <<- inverse
	}

	# get the inverse matrix
	getInverse<-function() {
		i
	}

	# return a list, each named element of the list is a function
	list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}


# ############################################################
#
# cacheSolve
#	function to calculate the inverse of a matrix
#	the function will return a cached version of the result if its been calculated before
#	
cacheSolve <- function(x, ...) {

	## get the cached inverse
	i <- x$getInverse()

	# No data cached as i is null so create and set the inverse
	if (is.null(i)) {
	    data <- x$getMatrix()
	    i <- solve(data, ...)

	    # Cache the inverse
	    x$setInverse(i)
	}
	else {
		message("getting cached data")
	}

	# Return the inverse
	i
}
