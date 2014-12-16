## Those two functions are used to create a special object that is used to store
## a matrix and caches its inverse.

## The first function takes a matrix and generates a list with functions to set 
## the value of the matrix, get the value of the matrix, set the value of the 
## inverse of the matrix, and get the value of the inverse already computed.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The second function is used to calculate the inverse of the matrix used as 
## an argument in the first functions. It either computes, stores and returns
## the inverse, or just fetch the inverse in the list created previously, if
## said inverse has already been computed.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}