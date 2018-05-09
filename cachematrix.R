## These two functions together find the inverse of a matrix and save it in the
##cache.  This way intensive computations can be bypassed if it is the same
##matrix being inverted

## makeCacheMatrix outputs a list of different elements including the 
# matrix (get), the inverse value (getInverse) and variables assigned to parent
# envrionments so they are accessible for the cacheSolve function.  In the process.events
# it creates functions in the list that the cacheSolve function can access

makeCacheMatrix <- function(x = matrix()) {
        
        #inv is cache value for inverted matrix
        inv <<- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
                #print(x)
        }
        
        get <- function() x
        setInverse <- function(inverse)  inv <<- inverse
        getInverse <- function() inv
        #print(inv)
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        #print(x)
}


## Cachesolves takes the output element of makeCacheMatrix and uses it to 
##determine if the inverse value has been cached.  It access the environment
##of makeCacheMatrix to use variables and function to determine if a
##cache value exists.  If so it states the cached value, if not it calclates
##the inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        message ("hello")
        x$setInverse(inv)
        inv
}