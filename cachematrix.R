## cachematrix.R R assignment 2
## There are two functions
## makeCacheMatrix() is being used to store a
## square matrix.  The assumption is that the matrix
## input is always a square matrix that is inversable
##
## cacheSolve() is used to return the pre-calculated inverse
## matrix if exist, otherwise it will calculate it and store
## it by setMInv()
##

## makeCacheMatrix() is the function being used to create
## the matrix and store the inverse
##
## ## It has 4 internal functions
##   set() initialize it
##   get() can be used to get back what is set
##   setMInv() is being used by another function to
##   set the inversed matrix
##   getMInv() will return the pre-calculated inverse matrix
##   or NULL
makeCacheMatrix <- function(x = matrix()) {
		## initialize the variable to store the inverse
		m <- NULL
		# init function set
        set <- function(y) {
                x <<- y
                m <<- NULL              
        }
        get <- function() x
        setMInv <- function(solve) m <<- solve
        getMInv <- function() m
        list(set = set, get = get,
             setMInv = setMInv,
             getMInv = getMInv)
}


## cacheSolve() is to return the cached solved Inverse matrix if available
## Otherwise, it will calculate it and store it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		    ## Assuming x is always inversable
        ## Attempt to get inverse of 'x'
        m <- x$getMInv()
        
        ## Test if the return is NULL or not
        if(!is.null(m)) {
                ## not null, return the inverse
                message("getting cached data")
                return(m)
        }
        ## null, calculate the inverse by first get 
        ## the original matrix using get(), store it in 
        ## local variable mat
        mat <- x$get()
        ## Then calculate the inverse and store in m
        m <- solve(mat)
        ## store m back to x to cache it
        x$setMInv(m)
        ## display m, the inversed matrix
        m		
}
