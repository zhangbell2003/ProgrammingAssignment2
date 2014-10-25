## Cache the inverse of a matrix
## Create a matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize the inverse
        i <- NULL
        
        ## set the matrix
        set <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }
        
        ## get the matrix
        get <- function() {
                ## Return the matrix
                m
        }
        
        ## set the inverse of the matrix
        setInverse <- function(inverse) {
                i <<- inverse
        }
        
        ## the inverse of the matrix
        getInverse <- function() {
                ## Return the inverse property
                i
        }
        
        ## Return a list of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. 

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## return the inverse if its already set
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        ## Get the matrix from our object
        data <- x$get()
        
        ## Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        
        ## Set the inverse to the object
        x$setInverse(m)
        
        ## Return the matrix
        m
}
