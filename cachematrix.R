## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##this function store the matrix in the cache
makeCacheMatrix <- function(x=matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinver <- function(inversa) inver <<- inversa
        getinver <- function() inver
        list(set = set, get = get,
             setinver = setinver,
             getinver = getinver)
}


## Write a short comment describing this function
## this function finds the inverse of the given matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getinver()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        mtx <- x$get()
        inver <- solve(mtx, ...)
        x$setinver(inver)
        inver
}
