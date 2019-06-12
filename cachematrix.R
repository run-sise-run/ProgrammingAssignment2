## These functiones combine to store a matrix and its inverse in a "cache"
## to save time in calculating a matrix'sinverse  repeatedly.


## Creates a vector of callable functions to set, get the original matrix
## and to set and get that matrix's inverse, stored in variables outside this environment.

makeCacheMatrix <- function(x = matrix()) {
    # x is the matrix; i is its inverse
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function () x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## returns inverse of a matrix, unless it is already stored in the "cache"
## in which case it returns teh cached value of this operation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached solution")
        return(inv)
    } else {
        mat <- x$get()
        inv <- solve(mat)
        x$setinv(inv)
        return(inv)
    }
}
