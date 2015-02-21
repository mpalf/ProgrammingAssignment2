################################################################################
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly.
## 
##  --Roger D. Peng
##
## The following functions were written as part of an online course in R.
## They cache the inverse of a matrix.
################################################################################


## This function creates a special "matrix" object that can cache its inverse.
## It is a slightly modified version of the 'makeVector' function provided in
## the example "Caching the Mean of a Vector".
## See https://github.com/rdpeng/ProgrammingAssignment2 
## 'set' function was removed since it isn't used. 

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL   # i is used as cache for the inverse matrix
                # it can only be set with the 'setinverse' function
	get <- function() x
	setinverse <- function(inv) i <<- inv 
	getinverse <- function() i
	list(get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## 'makeCacheMatrix' above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve retrieves
## the inverse from the cache.
## Same as the previous function, this one is also just a slightly modified
## version of the 'cachemean' function from the abovementioned example.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {   # i will not be NULL only if the inverse has previously
                        # been calculated (and cached with 'setinverse')
		message("getting cached data")
		return(i)       # return value read from cache
	}
	data <- x$get()
	i <- solve(data, ...)   # solve returns the inverse of an invertible matrix
	x$setinverse(i)         # cache the inverse matrix
	i
}
