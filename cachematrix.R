## In this Programming Assignment, we will take advantage of 
## the scoping rules of the R language and how they can be 
## manipulated to preserve state inside of an R object. Here,
##  we will write a pair of functions that cache the inverse 
## of a matrix to avoid costly re-computation 

## The 'makeCacheMatrix' function creates a list containing a 
## function to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

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

## The 'cacheSolve' function calculates the inverse of the 
## special "matrix" created with the makeCacheMatrix' function.
## However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the 
##inverse of the matrix and sets the value of the inverse in 
## the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)    
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
