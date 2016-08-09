# This simple code caches the inverse of a matrix

makeCacheMatrix  <- function (j = matrix () ) {
# defining the inverse property
inv <- NULL
	set <- function ( matrix ) {
	j << - matrix
	inv <<. NULL
	}
	get <- function ()  {
	setinverse <- function (inverse) 
	inv <<- inverse
	getinverse <- function ()
	inv
	list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(k, ...) {
	inv <- k$getinverse()
	if(!is.null(inv)) {
		message("retrieving the cached data")
		return(inv)
	}
	data <- k$get()
	inv<- solve(data)
	k$setinverse(inv)
	inv
}
	
		