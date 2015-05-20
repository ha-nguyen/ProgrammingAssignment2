#Author: Ha Nguyen 
#Course: R programming - Coursera 
## Create functions to cache potentially time-consuming computations 

## Function to create invertible matrix that can be cached its inverse.  

makeCacheMatrix <- function(x = matrix()) {
        inverse.x <- NULL 
        # function to set a value for matrix
        set <- function (y) { 
                x <<- y
                inverse.x <<- NULL 
        }
        # function to get the value of matrix 
        get <- function() x 
        # function to set matrix inverse 
        setInverse <- function(i) inverse.x <<- i
        # function to get matrix inverse 
        getInverse <- function() inverse.x 
        # return the list containing these functions above 
        list(set = set, get = get, 
             setInverseMatrix = setInverse, 
             getInverseMatrix = getInverse)
        
}


## Function to compute the inverse of the special "matrix" returned by function makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse.x <- x$getInverse()
        if (!is.null(inverse.x)) {
                message("getting cached data")
                return(inverse.x)
        }
        #if there's no cached data, calculate matrix inverse 
        data <- x$get()
        inverse.x <- solve(data)
        #set value of matrix inverse in the cache via setInverse function 
        x$setInverse(inverse.x)
        inverse.x
        
}

