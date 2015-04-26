## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inv <<- i
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# This function takes a matrix-list ( as created by the makeCacheMatrix function )
# and inverts it. If the inverse has been computed previously, it returns the previously
# computed value.
cacheSolve <- function(x, ...) {
  
        #TODO what happens if this is called with a different set of params than the
        #one used to compute the inverse originally ? e.g., if a different tol needs
        #to be specified ?
  
        inv <- x$getInverse()
        if( !is.null(inv)) {
            # the inverse has been previously computed, so just return that
            message("getting cached inverse")
            return(inv)
        }
        # implicit else. invert the matrix and store the results
        data = x$get()
        inv = solve(data)   
        x$setInverse(inv)
        inv
}
