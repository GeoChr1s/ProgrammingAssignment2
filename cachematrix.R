## the first function that creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(a = matrix()) {
        s <- NULL
        set <- function(b) {
                a <<- b
                s <<- NULL
        }
        get <- function() a
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## the second function computes the inverse of the special "matrix"  or checks if it has already been.
cacheSolve <- function(a, ...) {
        s <- a$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- a$get()
        s <- solve(data, ...)
        a$setsolve(s)
        s
}
