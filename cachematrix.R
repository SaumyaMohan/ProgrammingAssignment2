
## Put comments here that give an overall description of what your
## functions do
## These functions calculate the inverse of a matrix.
## When the inverse is calculated the first time, it is cached
## so that it doesn't need to be calculated again.

## Write a short comment describing this function
## Usage Example:
## cacheSolve(a)
## cacheSolve(a) (This time, the inverse is retrieved from the cache)

makeCacheMatrix <- function(x = matrix()) {
        ## makeCacheMatrix function takes in a matrix and when inverse is set, it stores the inverse in cache
        
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() {
                x
        }
        
        setinverse <- function(inverseval) {
                inverse <<- inverseval
        }
        
        getinverse <- function() {
                inverse
        }
        
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Write a short comment describing this function
## The cacheSolve function returns the inverse of a matrix.
## If the inverse is stored in cache, it retrieves from there, else calculates the inverse and returns that

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        
        if (!is.null(inverse)) {
                message("getting cached data for inverse")
                return(inverse)
        }
        
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}