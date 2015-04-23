## This is to provide functions for Caching the Inverse of a Matrix

## makeCacheMatrix function creates a special matrix, which is really a list 
## containing a function to: set the value of the matrix, get the value of the 
## matrix, set the value of the inverted matrix, get the value of the inverted
## matrix
makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(Inv) m <<- Inv
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## cacheSolve function is retirning inerted matrix. However, it first checks to 
## see if the inverted matrix has already been calculated. If so, it gets the 
## inverted marix from the cache and skips the computation. Otherwise, it 
## calculates the inverted matrix of the data and sets the value of the inverted
## matrix in the cache via the setInv function.
cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}

