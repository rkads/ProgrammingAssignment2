##cachematrix - A pair of functions that compute and return cached inverse of the a matrix; by -> rkads


##makeCacheMatrix - This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ##set the value of the special object matrix
        
        set <- function (y){
                x <<- y
                m <<- NULL
        }
        
        ## get the value of the matrix
        get <- function()x
        
        ##set the value of inverse
        setInverse <- function(inverse)m<<-inverse
        
        ##get the value of inverse
        getInverse <- function()m
        
        list(set=set, get=get, setInverse = setInverse, getInverse = getInverse)
}


##cacheSolve - This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##             If the inverse has already been calculated cachesolve returns the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ##check if cached computed value exists and retun inverse from cache
        if(!is.null(m)){
                message ("getting cached data")
                return(m)
        }
        
        ##compute inverse of matrix and set the cache
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
