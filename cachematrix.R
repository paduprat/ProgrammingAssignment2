## The functions below are used to create a matrix and calculate its inverse.

## The MakeCacheMatrix function receives a matrix as an argument and assigns it to the inner property 'x'.
## The function has methods to retrieve the matrix as well as to set e retrieve its inverse. When setting the inverse, it uses the inner scope to cache the data.
## These methods are all exposed to the user through the returned list object.

makeCacheMatrix <- function(x = matrix()) {
        
        invMatrix <- NULL
        
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        
        get <- function() x
        
        setinversematrix <- function(inverseMatrix) invMatrix <<- inverseMatrix
        
        getinversematrix <- function() invMatrix
        
        list(
                set = set, 
                get = get,
                setinversematrix = setinversematrix,
                getinversematrix = getinversematrix
        )
}


## The cacheSolve function receives the list object created by the MakeCacheMatrix and uses its embeded methods to calculate or return the inverse of the matrix.
## The function checks if the inverse has already been calculated and stored in cache. If it has, than the 'getinversematrix' method retrieves the inverse matrix. If it has not, the original matrix is retrieved and the function calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getinversematrix()
        
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        
        data <- x$get()
        
        invMatrix <- solve(data, ...)
        
        x$setinversematrix(invMatrix)
        
        invMatrix
}
