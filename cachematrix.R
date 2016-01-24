## The two functions below create a special object that stores a 
## matrix and cache's its inverse

## Function makeCacheMatrix creates a list containing a function to:
# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the inverse of the matrix
# 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- NULL
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function cacheSolve returns the inverse of a given matrix using the 
## special list created with makeCacheMatrix; it first tries to return
## the inverse from the cache; if there is no cached inverse, it calculates it

cacheSolve <- function(x, ...) {
    ## The objective is to return a matrix that is the inverse of 'x'
    ## where x is a special "matrix" object created from an invertible matrix
    ## using the makeCacheMatrix function above

    ## We first try to get the inverse from cache
    inv <- x$getinv()
    if(!is.null(inv)) {
        ## if the inverse is cached, we return it and exit the function
        message("getting cached data")
        return(inv)
    }
    ## If the inverse is not cached, the function continues: 
    ## the inverse is calculated, then cached and finally returned
    originalMatrix <- x$get()
    inv <- solve(originalMatrix)
    x$setinv(inv)
    inv
}

## To use, first we must first create the special "matrix" xMatrix using 
## the first function: xMatrix <- makeCacheMatrix(x)
## then we call cacheSolve(xMatrix)
## the first time, it will calculate the inverse
## if we call it again, it will take it from cache, and show the message
