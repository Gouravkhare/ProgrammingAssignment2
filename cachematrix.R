## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function create special matrix which cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        
        ## Set function set value of normal matrix to special matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get function return value of matrix
        get <- function() x 
        
        ## setsolve function cache inverse of matrix
        setsolve <- function(solve) m <<- solve
        
        ## getsolve function return cached value of inverse of matrix
        getsolve <- function() m
        
        ## makeCacheMatrix function return list of functions
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## cacheSolve function return inverse of matrix from cache, 
## if there is no value in cache then this function compute inverse
## and store in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## check if cached value available
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## else get the data and calculate inverse of matrix and store in cache
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}               

### Function to test above code:
### copy/paste following lines in R studio
testAssignment2 <-function(){
        message("Creating 2x2 cached matrix:")
        t1 <- matrix(1:4,2,2)
        t2 <- makeCacheMatrix(t1)
        
        message("Computing inverse first time:")
        print(cacheSolve(t2))
        
        message("computing inverse second time:")
        print(cacheSolve(t2))
}
testAssignment2()

##End