## Put comments here that give an overall description of what your
## functions do

## returns a list of functions used for handling a matrix and its inverse

makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    
    # sets the value y to the matrix
    set <- function(y){
        mat <<- y
        inv <<- NULL
    }
    
    # gets the matrix
    get <- function() mat
    
    # sets the value i to the inverse matrix
    setInverse <- function(i) inv <<- i
    
    # gets the inverse matrix
    getInverse <- function() inv
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## calculates de inverse of a matrix. If the inverse has already been calculated it returns a cached value  

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    # verify if the inverse of the matrix has been calculated
    if( ! is.null(inv) ){
        message("getting cached inverse matrix")
        return (inv)
    }
    
    m <- x$get()
    
    # calculates de inverse
    x$setInverse(solve(m))
    
    x$getInverse()
}
