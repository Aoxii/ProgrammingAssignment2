## Put comments here that give an overall description of what your
## functions do

#  Example :
# zz <- matrix(rnorm(16), nrow = 4)           # Create a matrix zz
# zzs  <- makeCacheMatrix(zz )                # Create the special mat
# zzs$get()                                   # the matrix
# cacheSolve(zzs)                             # inverse
# cacheSolve(zzs)                             # Call the 2nd time



makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv

    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x, ...) {
    inv <- x$getinv()

    # If the inverse is already calculated, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data, ...)

    x$setinv(inv)

    inv
}
