## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The 'makeCacheMatrix' function creates a special "matrix" object 
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL				##i will contain the inverse matrix
	set <- function(y) {	##
		x <<- y				##Assign y to x when a new matrix is defined in the parent environment
		i <<- NULL			##when a new matrix is defined, set i to NULL
	}
	get <- function() x		
	setinverse <- function(inverse) i <<- inverse		##assigns value of inverse in parent environment
	getinverse <- function() i						##gets the value of inverse when called
	list(set = set, get = get,
	     setinverse = setinverse , getinverse = getinverse)
}


## Write a short comment describing this function
##The 'cacheSolve' function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()							##
	if(!is.null(i)) {							##if i is not null, get data from cache
		message("getting cached data")
		return(i)
	}
	data <- x$get()								
	i <- inverse(data, ...)						##else, calculate the inverse
	x$setinverse(i)								
	i
}
