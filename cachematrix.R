## The makeCacheMatrix function sets the value
## of a matrix, gets the value of a matrix,
## sets the value of the inverse, and gets the
## value of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inverse) {
        i <<- inverse
    }
    getinverse <- function() {
        i
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function checks to see
## if the inverse of the matrix has already 
## been calculated, if so, it prints
## "getting cached inverse," and prints the
## inverse while skipping the calculation. 
## If not, it calculates the inverse of
## the matrix and sets the cached value.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached inverse")
        return(i)   
    }
    mat <- x$get()
    i <- solve(mat, ...)
    x$setinverse(i)
    i
}
