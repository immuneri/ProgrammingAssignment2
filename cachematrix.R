##1st function creates a special matrix object, 2nd computers the inverse of special matrix object (returned from the 1st function)
##if, however, the calculation has already been done and matrix hasn't changed, it is retrieved from the cache

##I took the code given in the assignment description and made necessary changes so that it can inverse matrix.
##Instead of "m" I used "cac", I also changed the "mean" to "solve" throughout the code


##1st function, creates spec. matrix object

makeCacheMatrix <- function(x = matrix()) {  
        cac <- NULL                             
        set <- function(y) {                    
                x <<- y                         ##assigns value to the object in the parent environment
                cac <<- NULL                    ##cac is set to NULL (in parent environment) each time new matrix object is created
        }
        get <- function() x                     ## to get the matrix 
        setsolve <- function(solve) cac <<- solve
        getsolve <- function() cac
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


##2nd function - calculates the inverse matrix or retrieves it from cache 

cacheSolve <- function(x, ...) {
        cac <- x$getsolve()             ##calculates the inversion of matrix and saves in cac
        if(!is.null(cac)) {             ##checks whether the cache (cac) is empty: if not empty, proceeds
                message("Retrieving cached inverse matrix")
                return(cac)             ##returns cached matrix without having the preform inversion again(and prints a message)
        }
        data <- x$get()                 
        cac <- solve(data, ...)         
        x$setsolve(cac)
        cac
        ## Returns a matrix that is the inverse of 'x'
}
