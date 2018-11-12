## Put comments here that give an overall description of what your
## functions do

## The first function creates a special matrix object that caches it's inverse
## The second function computes the inverse of the matrix created by the first function
## but retrieves this inverse directly from the cache if the matrix has not changed

## Write a short comment describing this function

## Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## Computes the inverse of the matrix calculated by makeCacheMatrix function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        return(inv)
}
