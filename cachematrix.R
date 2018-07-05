## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	
	## initialize inv as an empty matrix
	
	inv<- NULL

	## a function that caches the matrix 'newmatrix' and stores it as x

	set <- function(newmatrix){
		
		## assign newmatrix to x
		
		x <<- newmatrix
		
		## I think that the idea here is that when x is 'reset',
		## to newmatrix, we need to delete inv (i.e. by assigning NULL), 
		## since otherwise, inv would contain the inverse of the previous matrix
		
		inv <<- NULL
	}

	## get is a function that returns the cached matrix x

	get <- function() x

	## setinv is a function that takes an argument inverse, and caches it as inv

	setinv <- function(inverse) inv <<- inverse


	## getinv is a function that returns the cached matrix inv

	getinv <- function() inv

	## makeCacheMatrix returns a (named) list of the four functions defined here
	
	return(list(set=set, get=get, setinv = setinv, 
	getinv = getinv))
}


## Write a short comment describing this function

## The function cacheSolve takes a list of functions 'y' as its argument.
## This list must contain functions named 'getinv,' 'get' and 'setinv'.
## The idea is to apply this function to the output of makeCahceMatrix

cacheSolve <- function(y, ...) {

	## assigns the output of the function 'getinv' to the variable inverse.
	## There are two possibilities here; either the inverse has been computed 
	## before, in which case, inverse will get the cached inverse of the matrix x
	## otherwise, y$getinv will be NULL.
	
	inverse <- y$getinv()
	
	## The following conditional tests whether or not inv has been calculated previously. 
	## If not, then inverse will be NULL, so we proceed as before.
	## So if inverse is not NULL, this means that the inverse has already been computed.
	##  If this is the case then return the cached inverse.
	
	if(!is.null(inverse)){
		message("I've seen this before... no need to compute again... getting cached data")
		return(inverse)
	}

	## retrieves the matrix from makeCacheMatrix using the 'get' function,
	## and assigns this to the variable matrix

	matrix<- y$get ()

	## inverts matrix it and assigns this inverse to the variable 'inverse'
	
	inverse <- solve(matrix,...)
	
	## runs the setinv function from makeCacheMatrix,
	## i.e., stores inv to the cache
		
	y$setinv(inverse)
	
	## finally, this function returns the inverse of our original matrix.

	return(inverse)

}