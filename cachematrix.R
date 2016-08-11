# This simple code caches the inverse of a matrix
# The code defines two functions which are used to caching the inverse of the matrix

#makeCacheMatrix creates a list with a function which 
# 1. initially defines the value of the matrix
# 2. derive the value of the matrix
# 3. defines the inverse of the matrix
# 4. derive the values of the matrix
makeCacheMatrix  <- function (j = matrix () ) {
# defining the inverse property
inv <- NULL
# defining the matrix
	set <- function ( matrix ) {
	j <<- matrix
	inv <<- NULL
	}
# deriving the matrix
	get <- function ()  {
# defining the inverse of the matrix
	setinverse <- function (inverse) 
	inv <<- inverse
# deriving the inverse of the matrix
	getinverse <- function ()
	inv
	list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# In the following codes below, the function returns the inverse of the matrix and initially verify the computation of the inverse returned by "makeCacheMatrix".
# If the inverse has already been computed and the matrix has not been changed it retrieves the result and skip the computation
# In the case where the computation has not been done, it computes the inverse and defines the value in the cache using the setinverse funtion

cacheSolve <- function(k, ...) {
# return the matrix, that is, the inverse of 'k'
	inv <- k$getinverse()
# returns the inverse if this is already defined
	if(!is.null(inv)) {
		message("retrieving the cached data")
		return(inv)
	}
# derives the matrix from the object
	data <- k$get()
# calculates the inverse using matrix multiplication
	inv<- solve(data)
# defines the inverse of the object
	k$setinverse(inv)
# returns the matrix
	inv
}
	
		