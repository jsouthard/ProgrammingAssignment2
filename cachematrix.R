## The cachematrix functions implement a caching matrix inversion solver
## These functions take advantage of R's lexical scoping to establish
##     a cache for expensive operations

## makeCacheMatrix returns a list containing "getter" and "setter" functions
##     for the matrix and its cached inverse.
## Note: The cached inverse is set externally (through cacheSolve) and will
##     be NULL initially

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv)
}


## cacheSolve returns the inverse of a matrix contained in a makeCacheMatrix
##     structure using a cached value if it exists, 
##     initializing the cache otherwise
## Additional parameters to the solve() function can be passed after the
##     makeCacheMatrix structure x

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		return(inv)
	}
	
	# Moved notification to cache init, so the feedback exists for the long,
	#     infrequent operation
	message("cache miss, populating cache")
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
