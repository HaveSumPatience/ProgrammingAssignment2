## I've created two functions which will calculate the inverse
## of any square invertible matrix. If the inverse has already 
## been calculated, it will return the cache

## Write a short comment describing this function
## makeCacheMatrix will help get and set the actual matrix, as
## well as get its inverse
makeCacheMatrix <- function(x = matrix()) {
        #Set the value of the matrix
        i <- NULL
        set <- function(y) {
        x <<- y
        i <<- NULL
        }
        #Get the value of the matrix
        get <- function() {
        x
        }
        #Set the value of the inverse
        setinverse <- function(inverse) {
        i <<- inverse
        }
        #Get the value of the inverse
        getinverse <- function() {
        i
        }
        #Create a list containing the above functions
        list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
## cacheSolve will first check if the inverse has already been
## calculated, then return it. Otherwise, it will calculate the
## the inverse of the given matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #If inverse has already been calculated, return it
        i <- x$getinverse()
        if(!is.null(i)) {
        message("getting cached data")
        return(i)
        }
        #If no cache is available, calculate inverse
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
