## Function makeCacheMatrix is a special "Matrix" designed to cache its inverse. 
## makeCacheMatrix contains 4 functions: set, get, setinverse, getinverse.
##(1)get is a function that returns the matrix x stored in the main function.
##(2)set is a function that changes the matrix stored in the main function.
##(3)setinverse and getmatrix are functions that store the value of the input in a variable m.

makeCacheMatrix <- function(x = matrix()) {
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


## Write a short comment describing this function
##

cacheSolve <- function(x, ...) {
## The function "cacheSolve" returns a matrix that is
##the inverse of 'x'Function " returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix 
##has not changed), then the cachesolve should retrieve the 
##inverse from the cache.If the inverse has not been calculated, 
##data gets the matrix stored with makeCacheMatrix, m calculates the inverse, 
#and x$setmean(m) stores it in the object m in makeCacheMatrix.
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        }
