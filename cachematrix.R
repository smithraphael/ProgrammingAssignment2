## This function allows a best computational performance, using cache memory to capture inverse matrix 
## previously calculated.


## The makeCacheMatrix enclosures 4 functions described below
## set(): Set the value of the matrix and creates an environment (parent) to cache calculated values
## get():Get the value of the matrix
##setsolve(): Set the value of the solve (inverse matrix calculation)
##getsolve(): Get the value of the solve (inverse matrix calculation)


makeCacheMatrix = function (x = matrix()){
        
        s=NULL
        
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        get <- function() x
        setsolve <- function(solve) s <<- solve
        
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
        

## Check if the inverse of the matrix "x" is already calculated
## If it is calculated the function reads the data from Cache
## If it still not, then calculates the inverse of a matrix and include the result into Cache
## (if the matrix is invertible. Otherwise it returns an error)

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
