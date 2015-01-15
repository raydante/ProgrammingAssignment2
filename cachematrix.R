## makeCacheMatrix wraps a matrix in getter and setter functions while
## cacheSolve returns the inverse using cached results or solve()

## Takes a matrix x and wraps it in getter and setter functions for the 
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
            x <<- y
            i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
    }




## Takes the output of makeCacheMatrix and returns stored inverse if there, 
## otherwise finds inverse via solve() then caches and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        # message("using cached inverse")
        return(inv)
    }
    m8trix <- x$get()
    inv <- solve(m8trix,...)
    x$setInverse(inv)
    inv
}


