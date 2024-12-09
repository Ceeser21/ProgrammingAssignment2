## Two functions that speed up the process of inverting a matrix

## Caches a matrix and its inversion so it can be called again 
## without having to recalculate the inverse

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y){
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setInv <- function(solve) Inv <<- solve
        getInv <- function() Inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Generates the inverse of the matrix supplied by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getInv()
        if(!is.null(Inv)){
                message("Getting cached data...")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data)
        x$setInv(Inv)
        Inv   
}
