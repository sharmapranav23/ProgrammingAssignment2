## makeCacheMatrix() creates and returns a list of following 4 functions:
## set - set the value of the matrix
## get - get the value of the matrix
## setInverse - set the value of the Inverse of the matrix
## getInverse - get the value of the Inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
    
}


## cacheSolve function returns inverse of a matrix. The function first checks
##if the inverse has already been calculated. If yes, it simply returns the 
## already calculated (cache)  value. Else, it calculates the matrix (using solve())
## sets the value for future use and returns the value

cacheSolve <- function(x, ...) {
    
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
