## This function creates a special "matrix" object that can cache its inverse

## The first function, makeCacheMatrix creates a special "matrix" that is 
## invertible, and contains a function to
## set the value of the vector
## get the value of the vector
## set the value of the mean
#  get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        as.matrix(rnorm(m*m,mean=0,sd=1), m, m)
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting solved matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
