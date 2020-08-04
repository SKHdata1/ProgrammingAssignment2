##Cache the inverse of a matrix

##The cachematrix.R file contains two functions, makeCacheMatrix() and cacheSolve(). The first function in the file, makeCacheMatrix() creates an R object that stores a matrix and its inverse. The second function, cacheSolve() requires an argument that is returned by makeCacheMatrix() in order to retrieve the inverse from the cached value that is stored in the makeCacheMatrix() object's environment.

##Input invertible matrix, e.g. matrix(1:4, 2, 2).
##If non-invertable, then the following error occurs-- "Error in solve.default(data, ...):Lapack routine dgesv: system is exactly singular: U[1,1] = 0"

##Example
#source("cachematrix.R")
#set <- matrix(1:4, 2, 2)
#a <- makeCacheMatrix(set)
#a
#cacheSolve(a)
#cacheSolve(a)


## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

