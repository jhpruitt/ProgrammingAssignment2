## The makeCacheMatrix functions creates a list of function outputs
## that allow the user to set the matrix, retrieve the orinal matrix,
## store the inverse of the matrix, or retrieved the inverse of the 
## matrix. The cacehSolve function checks to see if the inverse of the
## matrix exists, and returns it if it does. Otherwise, it will create
## the inverse and cache it. 

## This function creates a special matrix and creates a list for 
## cacheSolve to refer to in searching for cached inverses. 

## JP - these functions seem to do everything needed in the assignment.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
   
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
  
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## This function checks for cached data and returns it, or 
## creates the inverse of a matrix and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) { ##need to be able to check if the inverse exists
        message("Getting cached data")
        return(m) ## return inverse if it is cahced
    }
    
    data <- x$get() ##if not chached, create inverse and cache it
    m <- solve(data)
    x$setmatrix(m)
     m    
    
}
