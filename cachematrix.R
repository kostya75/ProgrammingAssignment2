#Function creates a list that contains functions to:
#set matrix
#get matrix
#set the inverse
#get the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
    #set x to global environment
    #set inverse as null(empty)
        
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#cacheSolve function checks if inverse is already calculated for the matrix created above
#if no inverse (null), the function "gets" the matrix from the list and calculates the inverse

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
