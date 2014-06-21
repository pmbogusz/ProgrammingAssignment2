## Assignment: Caching the Inverse of a Matrix
## Coursera: R Programming
## https://class.coursera.org/rprog-004
## DATE: 	2014-06-21
## GITHUB:	https://github.com/pmbogusz/ProgrammingAssignment2

## Two functions that cache the inverse of a matrix to avoid renundand
## and costly computation 

## makeCacheMatrix: This function creates a special "matrix" object that
## can cache its inverse. This object containing:
##     4 functions: set_mx, get_mx, setinv, getinv
##     2 variables: x, inv

makeCacheMatrix <- function(x = matrix()) {
        
		# clearing cached inverse when creating new special "matrix" object
		inv <- NULL
		
		# seting new matrix and clearing cached inverse
        set_mx <- function(y) {
                x <<- y
                inv <<- NULL
        }

		# returning oryginal matrix  (not inverted)
        get_mx <- function() {
			x
		}
		
		# saving inverted matrix into "inv" variable
        setinv <- function(inverted) {
			inv <<- inverted
		}

		# returning cached inverted matrix 
        getinv <- function() {
			inv
		}
		
		#  list containing functions 
        list(set_mx = set_mx,
			 get_mx = get_mx,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated(and the matrix has not changed), then the cachesolve  retrieve
## shouldthe inverse from the cache

cacheSolve <- function(x, ...) {
		
		## Geting cached inverted matrix in to "inv" variable
		inv <- x$getinv()
		
		## if "inv" variable is not NULL then returning cached matrix
        if(!is.null(inv)) {
                message("getting cached inverted matrix")
                return(inv)
        }
		
		
		## If not (i.e. "inv"  is not NULL), then geting oryginal matrix
        data <- x$get_mx()
		
		## Invering it with "solve" function
        inv <- solve(data, ...)
		
		## caching with "setinv" function
        x$setinv(inv)
		
		## and finally returning it
        inv
		
}


