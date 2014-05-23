## Two functions that create a special "matrix" object that stores
## the inverse of a matrix R object assumed to be invertible.

## makeCacheMatrix creates a special "matrix", which is a list
## containing a function to set/get the value of a matrix and 
## set/get its inverse

makeCacheMatrix <- function(x) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve inverses the above makeCacheMatrix special "matrix" if
## it has not already been calculated

cacheSolve <- function(x) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

## Demonstration
TestMatrix <- matrix(1:4,2,2)
TestMatrix
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4

FuncMatrix <- makeCacheMatrix(TestMatrix)

cacheSolve(FuncMatrix) #1st run
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

cacheSolve(FuncMatrix) #2nd run
#getting cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
