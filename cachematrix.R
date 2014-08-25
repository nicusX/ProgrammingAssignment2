## The following two functions may be used to hold a matrix together with 
## its inverse
## makeCacheMatrix is the container of the data
## cacheSolve should be used to calculate/retrieve the inverse

## This function is a "container" holding a matrix and its inverse
## It retuirns a list of functions (set/get, setinv/getinv)
## respectively to store/retireve the matrix
## and to store/retrieve the inverse
## The matrix may be initialized on creation of a new instance, 
## passing it as argument of makeCacheMatrix()
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    # Store the matrix
    set  <- function(mtx) {
        x <<- mtx
        i <<- NULL
    }
    
    # Retrieve the matrix
    get <- function() x
    
    # Store the inverse
    setinv <- function(inv) i <<- inv
    
    # Retrieve the inverse
    getinv <- function() i
    
    # Return a list of functions to access the content
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function returns the inverse from a makeCacheMatrix instance
## lazily calculating it only on the first call
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv  <- x$getinv()
    if(!is.null(inv)) {
        message("cache hit")
        return(inv)
    }
    mtx <- x$get()
    inv <- solve(mtx,...)
    x$setinv(inv)
    inv
}
