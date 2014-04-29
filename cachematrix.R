## create two function that calculate and cache the inverse of a matrix

## make Cache Matrix caches the result of the inversion of the input matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInvMatrix <- function(solve) m <<- solve
    getInvMatrix <- function() m
    list(set = set, get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}


## cacheSolve function computes inverse matrix if not been computed before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInvMatrix()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInvMatrix(m)
        m
}
