# Matrix inversion is usually a costly computation and there may 
# be some benefit to caching the inverse of a matrix rather than 
# computing it repeatedly (there are also alternatives to matrix 
# inversion that we will not discuss here). Your assignment is to 
# write a pair of functions that cache the inverse of a matrix.

# Creating the "matrix" object that cache the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get,
         setinverse = setinv,
         getinverse = getinv)
}


# Compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i<- solve(data, ...)
        x$setinverse(i)
        i
}

#Example
mat <- matrix(rnorm(36),6,6)
cached<-makeCacheMatrix(mat)
cacheSolve(cached)


