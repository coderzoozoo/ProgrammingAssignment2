## The functions defined in this file compute the inverse of a matrix and
## store the result in the cache. When the function cacheSolve is invoked
## multiple times for the same input matrix, cached result is returned 
## after the first attempt.

## makeCacheMatrix takes invertible matrix as input, defines getter
## and setter methods for the input matrix and its inverse. The function
## returns a list of getter and setter methods for the input matrix and
## its inverse.

makeCachMatrix <- function(x = matrix()) {
        #Initialize the inverse to NULL
        I <- NULL
        # Set the value of vector x to y and change. 
        # Since input matrix has changed, the cached 
        # value of inverse of input matrix does not 
        # represent the correct the inverse of new 
        # matrix. So I, cached value of Inverse, is
        # set to NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        # get the input matrix
        get <- function() x
        
        # Cache the inverse of the input matrix.
        setinverse <- function(inverse) I <<- inverse
        
        # Return the cached value of the inverse
        getinverse <- function() I
        
        # Create a list of all the utilities defined above
        # Since this is the last statement of the function,
        # it will be returned by the function, makeCachMatrix, 
        # as output
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes a special matrix, which is a list and 
## the output of makeCachMatrix, as input. If the inverse 
## of a matrix is cahced, the function returns the cached 
## value. Otherwise the function  calculates the inverse 
## and caches it using the utility function, setinverse. 
## The function outputs the inverse of a matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Fetch the cached value of inverse of matrix x
        I <- x$getinverse()
        
        # If the inverse is found in the cache, return the
        # cached value
        if(!is.null(I)) {
                message("getting cached value of Inverse")
                return(I)
        }
        
        # Get input matrix
        data <- x$get()
        
        # Calculate the inverse of imput matrix.
        I <- solve(data, ...)
        
        # Cache the calculated value of inverse of
        # input matrix
        x$setinverse(I)
        
        # Output the calculated inverse of input matrix
        I
}
        