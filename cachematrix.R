# computes the inverse of a matrix and then caches the results so that subsequent calls will be quicker
# to test:
#
# a <- matrix(c(8, 5, 77, -856), nrow = 2, ncol = 2)
# cm <- makeCacheMatrix(a)
# ccm <- cacheSolve(cm)	
#
# if you run ccm <- cacheSolve(cm) a second time you should get "getting cached data" in the output
#

# sets up the matrix with caching capabilities
makeCacheMatrix <- function(x = matrix()) {

	# stores the cached matrix inverse. first runs of the function will the cache will be null until we set it.
	inverseMatrix <<- NULL

    set <- function(y) {
            x <<- y
            inverseMatrix <<- NULL
    }
    get <- function() x	

    # sets the inversed data into cache
    setinverse <- function(inverse) inverseMatrix <<- inverse

    # gets the cached data
    getinverse <- function() inverseMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# returns the inverse of a square matrix. it will cache the results and return those... so it is quick to call
# multiple times
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        # attempt to get the data. if it's cached, it won't be null.
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()

        # performs the inverse operation
        m <- solve(data, ...)
        # store the results so it makes it into the cache
        x$setinverse(m)
        m        
}