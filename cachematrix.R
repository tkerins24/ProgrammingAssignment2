## Put comments here that give an overall description of what your
## functions do

#These functions when used together are used to streamline the calculation
#of the inverse of a square matrix by calculating the inverse of a matrix
#this first time,and then caching the results, and using the cache on 
#subsequent calls to avoid having to re-calculate the inverse thereafter.
#This saves alot of time, since the inverse does not haveto be calculated
#more than once. Assumption: the square matrix is invertible.

## Write a short comment describing this function

#The makeCacheMatrix function creates a special matrix object that can
#cache it's inverse. It returns a list of functions shown below that
#are used as input by the cacheSolve function:

#1) set the value of the matrix
#2) get the value of the matrix
#3) set the value of the inverse of the matrix
#4) get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(matrix_inverse) inverse <<-matrix_inverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
#The cacheSolve function takes the output from the special matrix returned 
#above and calculates the inverse of it. The first time through it will 
#calculate the inverse. If the inverse has been previuosly calculated it
#will retrieve the inverse from the cache. Assumption: the matrix has not
#changed from when makeCacheMatrix was calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting inverse from cache")
                return(inverse)
        }
         data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}
