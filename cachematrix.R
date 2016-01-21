##  The function makeCacheMatrix takes in a matrix, which is assumed to be invertible.
##  No check is made to see if the input is square.

## The default matrix is a simple 2X2 identity matrix, which can be solved without error.

## The output is a list containing functions to
#   1.  set the value of the input matrix.
#   2.  get the value of the stored matrix.
#   3.  set the inverse of the input matrix
#   4.  get the the stored value of the inverse.

makeCacheMatrix <- function(x = matrix(c(1,0,0,1),nrow = 2)){
        m <- NULL
        set <- function(y){
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
##------------------------------------------------------------------

## The function cacheSolve takes as its input the output
##  of the above function makeCacheMatrix.

##  If the inverse value of the input is found not to be NULL
##  then that value is returned,
##  otherwise the inverse of the original matrix is calculated
##  and stored in x.
##  The inverse of the original matrix is returned.

cacheSolve <- function(x, ...){
        m <- x$getinverse()
        if (!is.null(m)){
                message("getting cached inverse solution")
                return (m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
