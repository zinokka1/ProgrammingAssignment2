## This function creates a special "matrix" object that can cache its inverse
##Current function creates a matrix obj than stores cache its inverse, it defines areguments with default mode
##of matrix, initializws inv as NULL; will hold value of matrix inverse 
##define the sett function to assign new value of matrix in parent environment
##if there is a new matrix and resets inv to null

makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL                             
    set <- function(y) {                    
        x <<- y                             
        inv <<- NULL                        
    }
    get <- function() x                     
    
    setinverse <- function(inverse) inv <<- inverse  
    getinverse <- function() inv                     
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
                                                                                  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
