## the following  pair of functions allow to cache the
## inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix takes a matrix as a parameter, saved in the private variable x
## and returns a list of public functions

makeCacheMatrix <- function(x = matrix()) {
	##initializing the inverse variable to NULL
	##we do this so that if getinverse() is called
	##after the makeCacheMatrix object is contructed
	##but without a call to the setinverse function
	##we then start with calculating the inverse of the matrix in 
	##the cacheSolve function.
	inv <- NULL
	
	##using the <<- super assignment operator, we set values for x and inv in the
	##enclosing environment (the one created when makeCacheMatrix was called)
	##and not in the environment created
	##with the call to the function set()
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	
	get <- function()x
	
	##computing the inverse of the matrix with the solve function
	##and storing it the inv variable defined in enclosing environment
	setinverse <- function(solve) inv <<- solve
	
	##this returns the inverse of the matrix
	getinverse <- function() inv
	
	##list returned by the makeCacheMatrix function
	##this list contains public functions accessible with
	##the $ operator. 
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##cachesolve() takes as parameter a special matrix created
##with the makeCacheMatrix() function
##cachesolve() computes the inverse of the special "matrix" returned by makeCacheMatrix
##above. If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	##we try to compute the inverse of the object created
	##with the makeCacheMatrix function
	inv <- x$getinverse()
	
	##if we have already computed the inverse of the matrix and
	##stored it via setinverse(), the variable inv is NOT NULL
	##and we can return the cached version of inv
	if(!is.null(inv)){
		message("getting cached data")
		##we use return to stop the execution of the function after the return.
		return(inv)
	}
	##in the following case, we have not already computed the
	##inverse so we do it now
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	##we return the computed inverse matrix
	inv
}

##Sample for testing :
##x <- matrix(c(1,2,3,4), nrow=2, ncol=2)
##matrice <- makeCacheMatrix(x)
##matrice$get() will display the matrix
##cacheSolve(matrice) will compute the inverse matrix and cache the result
##cacheSolve(matrice) called a second time will pull out the cached version
