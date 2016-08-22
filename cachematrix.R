## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix contains a function that returns a list to set
## the value of the matrix, get the value of the matrix, set the
## value of the inverse, and get the value of the inverse.
## minv: matrix inverse
## mval: matrix value

makeCacheMatrix <- function(x = matrix()) {
	minv <- NULL
	set <- function(mval) {
		x <<- mval
		minv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) minv <<- solve
	getinv <- function() minv
	list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve calculates the inverse of the matrix created in
## makeCacheMatrix, but also checks to see if this has already
## been calculated. If yes, the process is skipped. If no, it
## continues and sets the value in the cache as setinv.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getinv()
        if(!is.null(minv)) {
        	message("getting cached data")
        	return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setinv(minv)
        minv
}
