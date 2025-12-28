## Put comments here that give an overall description of what your
## functions do
## The following fucntions solve for the inverse of the matrix and since calculating inverse is computationally expensive 
##        we introduce another function that is makecachematrix which basically sees if the inverse has already been calculated or not
##        if it is already calculated it will be located in the cache memory, so it returns that. Otherwise if the inverse is not present
##        it stores the calculated inverse in cache memory for future use

## Write a short comment describing this function
## Creates a special "matrix" object that can cache its inverse.
## This function returns a list of functions to:
##   - set: Set the value of the matrix
##   - get: Get the value of the matrix
##   - setinverse: Set the value of the inverse matrix
##   - getinverse: Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Write a short comment describing this function

## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix hasn't changed),
## then cacheSolve retrieves the inverse from the cache, saving computation time.
## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
