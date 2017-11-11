## makeCacheMatrix object
## Contains four methods:
## (1) set - to set value of uninverted matrix
## (2) get - to retrieve uninverted matrix value
## (3) setinverse - to set the cached value of the inverted matrix
## (4) getinverse - to retrieve the cached value of the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve function
## Retrieves the inverse matrix value for the provided makeCacheMatrix argument
## If not cached inverse matrix value exists, calculates the inverse, then
## returns and caches the value
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}