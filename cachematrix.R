## Below are two functions that are used to create a special object 
## that stores a numeric matrix and cache's its invert.

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to

## set the matrix
## get the matrix
## set the invert of the matrix
## get the invert of the matrix

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinvert <- function(solve) i <<- solve
    getinvert <- function() i
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
    
}


## The following function calculates the invert of the matrix
## created with the above function. However, it 

## first checks to see if the invert has already been calculated. 
## If so, it gets the invert from the cache and skips the computation. 

## Otherwise, it calculates the invert of the matrix and sets the 
## invert of the matrix in the cache via the setinvert function.


cacheSolve <- function(x, ...) {

    i <- x$getinvert()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinvert(i)
    i
}