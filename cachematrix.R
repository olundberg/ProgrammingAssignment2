## makeCacheMatris takes one argument, which is the matrix that we want to
##invert.

## cacheSolve checks wethever the inverse have been calculated before
## and stored in chache, if it has, then the function gets the inverse
## from the cache so it doesn't have to be calculated again.

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize an empty object for the inverse
        inverse <- NULL
        
        ## Sets the x and inverse objects
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        ## Anonymous function to get, set and inverte the matrix passed
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        
        ## Returns a list with the functions set, get, setinverse and getinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Gets the inverse from the getfunction, calculated or not calculated
        inverse <- x$getinverse()
        
        ## If the inverse already is calculated, this satement returns
        ## the cached data so that we don't have to calculate it again
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        ## If the inverse haven't been calculated it has to be calculated
        ## and then returned
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
