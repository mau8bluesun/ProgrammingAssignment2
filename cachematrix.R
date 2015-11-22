## This functions are writing to improve time consuming computations, by put
## some results that no change, on a cache
# Sorry by my poors comments, but english it's not my native language

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        xinv <- NULL # xinv is where the result of inversion will be stored
        set <- function(y) {
                x <<- y
                xinv <<- NULL # it initializes xinv to null
        }
        
        get <- function() x # get the input matrix
        setInv <- function(inv) xinv <<- inv # set the inversed matrix
        getInv <- function() xinv # get the inversed matrix
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


cacheSolve <- function(x, ...) {
        m <- x$getInv() # get the inversed matrix from x
        if(!is.null(m)) { # if the inversion result exist, 
                message("getting cached data")
                return(m) # return the calculated inversion
        }
        data <- x$get() # if not exist, do x$get to get the matrix object
        m <- solve(data) # we solve it
        x$setInv(m) # we then set it to the object
        m # print the solved result
}
