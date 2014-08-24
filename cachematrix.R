## makeCacheMatrix returns a list of four functions to cache the inverse for matrix argument. 
## cacheSolve function takes special "matrix" object as argument and check if the inverse is 
## cached. If not, calculate the inverse for the matrix and cache the inverse. Otherwise, return
## the cached inverse. 

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list (set = set, get = get, setInverse = setInverse, 
	getInverse = getInverse)
}


## cacheSolve: This function checks if the inverse for the speical 
## matrix calculated and the matrix has not changed. If so, it returns 
## the cached inverse. Otherwise, it calculates the inverse of the 
## matrix and caches it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if (!is.null(inv)) {
		message("Retrieve inverse from cache")
		return(inv)
	}
	mx <- x$get()
	inv <- solve(mx, ...)
	x$setInverse(inv)
	inv
	
}
