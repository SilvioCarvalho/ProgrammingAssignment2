## This function is intended to save computational time
## by caching the inverse of a [inversible] matrix
## 
## Note of Caution: This function assumes that the matrix is inversible!
## 


## makeCacheMatrix creates a special ''matrix'', which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        setSolve <- function(Solve) inverseMatrix <<- Solve
        getSolve <- function() inverseMatrix 
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## cacheSolve calculates the inverse matrix of the special ''maxtrix'' created with the above function. 
## However, it first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse matrix of the data and sets the value of the inverse matrix in the cache via the 

setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	tryCatch({
		inverseMatrix <- x$getSolve()
        	if(!is.null(inverseMatrix )) {
            	message("getting cached data")
                	return(inverseMatrix)
        	}
        	inverseMatrix <- solve(...)
        	x$setSolve(inverseMatrix)
        	inverseMatrix
	}, error = function(ex) {
  			message("no data cached yet")
	}, finally = { }
	) ## End of tryCatch
}
