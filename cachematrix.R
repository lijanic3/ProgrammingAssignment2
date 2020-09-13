## There are two conjunctive functions: makeCacheMatrix, cacheSolve

## makeCacheMatrix consists of set, get, set.inv, get.inv
## makeCacheMatrix creates a matrix with its inverse form obtained

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL                             #initializing inverse as NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}                   #function to get matrix x
        set.inv <- function(inverse) {inv <<- inverse}
        get.inv <- function() {inv}             #function to obtain inverse of the matrix
        list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
}


## cacheSolve retrieves the inverse matrix obtained in makeCacheMatrix and calculates its value

cacheSolve <- function(x, ...){                 
        inv <- x$get.inv()                      #gets cache data
        if(!is.null(inv)){                      #check whether inverse is NULL
                message("getting cached data")
                return(inv)                     #return inverse value
        }
        mat <- x$get()
        inv <- solve(mat, ...)                  #calculates inverse value
        x$set.inv(inv)
        inv
}
