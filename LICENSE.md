## The following process is to compute the inverse of a matrix by using caching in the process;
## To avoid the time consuming process of computing the inverse of a matrix by using repeated loops,
## it would be more efficient to cache the inverse of the matrix.
## The following process and codes, will cache the inverse of a matrix:

## The "makeCacheMatrix" function is to build up a new matrix for the purpose of cacheing its inverse: 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}




## After building the matrix by using the "makeCacheMatrix" function,
## The following "cacheSolve" function will compute its inverse.
## So, it will import the inverse when it is already been computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("Getting The Cached Data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
