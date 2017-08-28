## The functions below will create a matrix and cache its inverse. If the inverse has not been cached,
## it'll be calculated.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL #creating the variable "inverse"
    
    set <- function(y) { #Define the function "set": assign new matrix in its parent environment
        x <<- y
        inverse <<- NULL
    }
    
    get <- function () x #Define the function "get": will return the matrix
    
    setInverse <- function (inv) inverse <<- inv #Define the function "setInverse": assign value of inverse in parent environment
    
    getInverse <- function () inverse #Define the function "getInverse": will return "inverse"
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) #This is the result of this function. This is needed for "cacheSolve" function below
}


## cacheSolve computes the inverse of the matrix created from the function above. If inverse is calculated already, it'll return the value from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse() #assigns the value i from the makeCacheMatrix result above
    
    if(!is.null(i)) { #determines if the inverse is calculated from the function above; if so, will return the value from the above function along with the message "cached data"
        message("cached data")
        return(i)
    }
    
    data <- x$get() #this will execute if the inverse was not calculated from above: assigning the cached matrix (from above) to a variable "data"
    
    i <- solve(data, ...) #calculating the inverse of the matrix
    
    x$setInverse(i) #storing the inverse, "i", calculated from the above line
    
    i #returns the result
}
