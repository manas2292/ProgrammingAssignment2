## This function caches the inverse of a matrix, if it is being repeated.

## makeCacheMatrix creates a list that contains a function to set the value of the vector, get the value of the vecor
## Set the value of the inverse and to get the value of the matrix inverse. 

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y){
                        x <<- y
                        inv <<- NULL
                }
                get <- function(){
                        x
                }
                setinv <- function(solve){
                        inv <<- solve
                }
                getinv <- function(){
                        inv
                }
                
                list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function checks if the inverse is already present; if it is it gets the value stored, else it calculates the inverse and sets the value.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...) 
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
