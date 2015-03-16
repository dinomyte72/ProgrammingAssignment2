## Put comments here that give an overall description of what your
## functions do

## The following function, called makeCacheMatrix, 
## will create a list of functions to get and set the inverted 
## matrix in cache.

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the cached matrix to NULL.
        cache <- NULL
        
        ## Set the value of the matrix.
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        
        ## Get the value of the matrix.
        get <- function() x
        
        ## Invert the matrix and store in the cache.
        setcache <- function(inverse) cache <<- inverse
        
        ## Get the inverted matric from cache.
        getinv <- function() cache
        
        ## Allow the above functions to used by other functions.
        list(set = set, get = get,
             setcache = setcache,
             getinv = getinv)
}


## The following function, called cacheSolve, will solve the inverse
## of the cached matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache <- x$getinv()
        
        ## Load matrix data only if the inverse has not yet been
        ## calculated.
        if(!is.null(cache)) {
                message("getting cached data")
                return(cache)
        }
        
        ## Create the matrix.
        matrix <- x$get()
        
        ## Solve the inverse of the matrix.
        cache <- solve(matrix, ...)
        
        ## Set the inverted matrix in cache.
        x$setcache(cache)
        
        ## Return value of cache.
        return(cache)
}