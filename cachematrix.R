## makeCacheMatrix will create a matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	i = NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
}
	get <- function() x
	setinv <- function(inv) i <<- inv
	getinv <- function() i
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve will return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
	i <- x$getinv()
	## checks to see whether the inverse has already been created and, if it has, returns the inverse from the cache
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data,...)
	x$setinv(i)
	i
}
