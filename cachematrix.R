## Function makeCacheMatrix(x = matrix()):
##      Stores inverse of matrix with get and set functions
## Arguments: 
##      x: matrix
## Local variables:
##      i: inverse of matrix or NUll
## Local functions:
##      set(y): assigns a new matrix
##      get(): returns matrix
##      getinverse(): returns inverse of matrix
##      setinverse(inverse): assigns inverse of matrix
##      list(...): pointers to local functions

makeCacheMatrix <- function(x = matrix()) {    
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse 
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function cacheSolve(x = makeCacheMatrix(x = matrix()))
##      Finds square matrix inverse, and and stores inverse.
##      Returns the inverse matrix and indicates if "getting cached data"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
mdat <- matrix(c(4,7, 2,6), nrow = 2, ncol = 2, byrow = TRUE)
cm <- makeCacheMatrix(mdat)
cm$get()
cm$getinverse()
cacheSolve(cm)
