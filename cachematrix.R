## Peer Graded Assignment Week 3
## A pair of functions that cache the inverse of a matrix

##  makeCacheMatrix function creates a special 
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	## inv stores the inverse of the matrix initialized as NULL
	inv <- NULL
	
	## set function sets the matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	## get function returns the matrix
	get <- function() x

	## setinv sets the inverse of the matrix
	setinv <- function(inv_matrix) inv <<- inv_matrix

	## getinv returns inverse of the matrix
	getinv <- function() inv
	
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	
	## return inverse of matrix if already in cache
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	
	## get matrix from above created object
	data <- x$get()
	
	## use solve function to calculate inverse of matrix
	## NOTE: solve(data) %*% data is said to give better results
	## solve(data) is used here as stated in problem statement 
	inv <- solve(data)

	## set the inverse of matrix
	x$setinv(inv)

	## return the inverse of the matrix
	inv
}
