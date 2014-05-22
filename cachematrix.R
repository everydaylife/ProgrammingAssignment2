## Howdy classmate,
## This is a programming assignment for Peng's R Programming Course
## Here's the summary: the first function creates a matrix
## The second function checks if we have previously calculated the inverse of the created matrix 
## If so, the second function pulls the cached value, otherwise, it calculates the inverse of the matrix
## Together, these two functions help to save on computational power

## makeCacheMatrix simply creates our matrix object

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}    


## cacheSolve calculates the inverse of matrix created by makeCacheMatrix
## It returns a matrix that is the inverse of 'x'
## If already calculated for said matrix, the value is pulled from the cache

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

