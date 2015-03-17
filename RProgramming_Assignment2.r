# The function makeCacheMatrix works by setting the value of a matrix,getting the value of the matrix, setting
# the value of inverse of the matrix, and finally getting the value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse.matrix <- NULL
    set.matrix <- function(y) {
        x <<- y
        inverse.matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse.matrix <<- inverse
    getinverse <- function() inverse.matrix
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The cacheSolve function returns the inverse of the matrix. The first step is to check and see if the inverse of the matrix
# has already been computed. If the matrix has already been computed, then it skips the computation and retuns the inverse.
# If the inverse has not been computed, then it computes the inverse and then places the value in the cache.

cacheSolve <- function(x, ...) {
    inverse.matrix <- x$getinverse()
    if(!is.null(inverse.matrix)) {
        message("getting cached data.")
        return(inverse.matrix)
    }
    data <- x$get()
    inverse.matrix <- solve(data)
    x$setinverse(inverse.matrix)
    inverse.matrix
}

#Sample Run of makeCacheMatrix
# x <- rbind(c(5,12), c(33, 18))
# z = makeCacheMatrix(x)
# z$get()
#      [,1] [,2]
#[1,]    5   12
#[2,]   33   18

# We will use cacheSolve to compute the inverse (if not already computed)
# cacheSolve(z)
#            [,1]        [,2]
#[1,] -0.05882353  0.03921569
#[2,]  0.10784314 -0.01633987

