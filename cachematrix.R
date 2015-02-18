## these functions help cache inverse of a matrix.
## in case inverse is called repeatedly, cached value is returned. 


## This function creates a matrixCache object which stores the inverse of a matrix. 
## The object also provides methods to access and set both the original matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
			x <<- y
			inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## This function expects an object returned by makeCacheMatrix and returns its inverse
## in case of inverse is already stored, it is returned. Else a new inverse is calculated and set for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		message("not in cache")
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
