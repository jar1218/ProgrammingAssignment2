## wrapper function for a matrix. stores matrix values and inverse 
##the matrix, if already solved. Also, function to caculate in the inverse and store it.
## JAR 09/26/2015

## This is the wrapper funciton for the matrix

makeCacheMatrix <- function(x = matrix()) {
	##initialize the inverse value as null, because it has not been caculated
	i <- NULL
	##get/set for matrix
	set <- function(y){
		x <<- y
		m <<- NULL ##set to null to clear and previous value
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## function to check for computed inverse, and if not found, compute 
##and save it to cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## start by calling the getinverse function
        i <- x$getinverse()
        ##Did it return anything?
        if (!is.null(i)) {
        	##yes, it did
        	message("getting cached data")
        	return(i)
        }
        ##no, it did not -- so let's calculate
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i       
}
