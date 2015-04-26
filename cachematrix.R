## This is R Programming Assignment 2: Lexical Scoping: Caching the Inverse of a Matrix
## This Assignment contains two functions namely makeCacheMatrix and cacheSolve. The makeCacheMatrix
## function creates a special "matrix" object that can cache its inverse. Calculating matrix inversion 
## is a computation-intensive and time-consuming process and thus there are benefits of caching the inverse 
## rather than computing it repeatedly. The second function cacheSolve computes the inverse of the special
## matrix returned by makeCacheMatrix function. However, this function first checks to see if the matrix inverse 
## has already been computed. If yes, then cacheSolve function retrieves the matrix inverse from the cache and
## skips computing it. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in 
## the cache.

## The makeCacheMatrix function creates a special "matrix" object, which contains a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
	    x <<- y
	    i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(
	set = set,
	get = get,
	setinverse = setinverse,
	getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the special "matrix"
## This function first checks to see if the matrix inverse has already been computed. If yes, then 
## cacheSolve function retrieves the matrix inverse from the cache and skips computing it. Otherwise,
## it calculates the inverse of the matrix and sets the value of the inverse in the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
	    message("getting cached data")
	    return(i)
	}
	data <- x$get()
	i <- solve(data,...)
	x$setinverse(i)
	i
}
