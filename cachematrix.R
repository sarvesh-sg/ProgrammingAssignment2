## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	# create a variable to store the cache
	cacheMatrix <- NULL

	#set function to store the given matrix in cache and initialise the result to null
	set <- function(argMatrix) {
		x <<- argMatrix
		cacheMatrix <<- NULL
	}

	# get function returns the stored matrix
	get <- function() x

	# setCacheinverse function shall store the inverse of matrix in cache
	setCacheinverse <- function(inverse) {
		return(cacheMatrix <<- inverse)
	}

	# getCacheinverse returns the matrix from cache
        getCacheinverse <- function() {
		return(cacheMatrix)
	}
	# the return object is the list of all functions of makeCacheMatrix 
	list( set = set, get = get,
	      setCacheinverse = setCacheinverse,
	      getCacheinverse = getCacheinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  	## Return a matrix that is the inverse of 'x'

	#Argument x is the special object (list) as created by the makeCacheMatrix function

	#To compute the inverse of matrix, example as shown below	
	#x<-makeCacheMatrix(rbind(c(1,-1/2),c(1,-1/4)))
	#cacheSolve(x)

	#check if matrix is already cached
  	cacheMatrix <- x$getCacheinverse()
	#if yes return the cached matrix
	if (!is.null(cacheMatrix)) {
		message("getting cached data")
		return(cacheMatrix)
	}	
	#if not cached yet, get the matrix using get function of makeCacheMatrix
	data <- x$get()
	#compute the inverse using solve() function
	#no checks have been performed on the data, it is assumed to be invertible matrix
	invData <- solve(data)
	#set the cache upon computation
        x$setCacheinverse(invData)
	#return the computed data
	return(invData)
}
