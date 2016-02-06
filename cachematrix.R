## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
	set =function (y) {
		x <<-y
		invrs <<- NULL
	}
	get =function() x
	setinv <- function(inverse)  invrs <<-inverse
	getinv <- function() invrs
	list(set=set,
	 	 get =get,
	 	 setinv = setinv,
	 	 getinv = getinv)
}


## This function computes the inverse of the special matrix created 
## by makeCacheMatrix above.  If the inverse has already been calculated
## and the matrix has not changed, it should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs <- x$getinv()
        if (!is.null(invrs)) {
        	message ("getting cached data")
        	return(invrs)
        }
        mat.data <-x$get()
        invrs =solve(mat.data, ...)
        x$setinv(invrs)
        invrs
}
