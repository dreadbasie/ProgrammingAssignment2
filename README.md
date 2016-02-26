makeCacheMatrix creates a “matrix” that can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            set_inverse <- function(solve) m <<- solve
            get_inverse <- function() m
            list(set = set, get = get,
                 set_inverse = set_inverse,
                 get_inverse = get_inverse)
    }


cacheSolve finds the inverse of the "matrix" output by makeCacheMatrix above. cacheSolve then checks to see if the inverse has already been calculated and finally, cacheSolve gets the inverse from the cache.

cacheSolve <- function(x, ...) {
            m <- x$get_inverse()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$set_inverse(m)
            m
    }


