## Matrix inversion is usually a costly computation and ther may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
## this code provides a pair of functions that cache the inverse of a matrix 

## The first function makeCacheMatrix creates a special "matrix" which is really 
## a list containing functions to 
## 1.set the value of the matrix 
## 2.get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y)
	{
		x <<- y
		i <<- NULL
	}
	
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set,get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}

 


## The second function cacheSolve calculates the inverse of the 
## special " matrix" created with the above function 
## however it first checks to see if the inverse has been calculated
## if so it gets the inverse from the cache
## and skips the computation
## otherwise it calculates the inverse of the matrix and sets 
## the value of the inverse in the cache via the setinverse function


cacheSolve <- function(x, ...) 
	{
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()
		if(!is.null(i))
		{
			message("CACHED INVERSE")
			return(i)
	
		}
		dat <- x$get()
		i <- solve(dat,...)
		x$setinverse(i)
		i
	}
