## makeCacheMatrix(): creates an matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
        ## x is a square invertible matrix
        ## returns a list of functions to
        ## a. set the matrix
        ## b. get the matrix
        ## c. set the inverse
        ## d. get the inverse
  
        inv <- NULL
        set <- function(y){
          x <<- y
          inv <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list (set = set, get = get, 
              setinverse = setinverse,
              getinverse = getinverse)
}


## cacheSolve(): computes the inverse of a matrix returned by mackeCacheMatrix().
## if there are no changes to the matrix and the inverse has already been calculated it retrieves from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
  
        inv = x$getinverse()
        if (!is.null(inv)){
        # get it from the cache and skips the computation. 
        message("getting cached data")
        return(inv)
}
  
        # otherwise, calculates the inverse 
        data = x$get()
        inv = solve(data, ...)
  
        # sets the value of the inverse in the cache via the setinv function.
        x$setinverse(inv)
        return(inv)
  }
