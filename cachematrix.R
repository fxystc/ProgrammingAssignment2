## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this functiont takes a Matrix and converts into a special list object which 
## essentially contains four functions set, get, setInv, getInv.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) m <<- Inv
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function
## this function takes the special list object created by makeCacheMatrix and
## returns inverse of the matrix if it exist; else, it calls the function solve
## function to calculate inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInv(m)
        m
}
