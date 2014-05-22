## makeChascheMatrix creates a special "matrix" object that can cache
## its inverse. The special object is a list of containing the following values:
## Set: Sets the value of the matrix
## Get: Gets the value of the matrix
## Setinverse: Sets the value of the inverse matrix with solve
## Getinverse: Gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL                                         ## m is variable that will store the inverse matrix calculation
                                                          ## m is initialized to NULL
        
        set <- function(y) {                              ## set: Caches the value of the initial matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                               ## get: Assigns the value of a matrix 
        setinverse <- function(solve) m <<- solve         ## setinverse: Calculates the inverse and caches it in variable m
        getinverse <- function() m                        ## getinverse: Returns the cached inverse matrix calculation 
        
        list(setmatrix = set, getmatrix = get,            ## makeCacheMatrix returns a list that stores the matrix value and
             setinverse = setinverse,                     ## its cached inverse
             getinverse = getinverse)
}



## cacheSolve is a function that calculates the inverse of a matrix. It starts by checking 
## whether the inverse has been already calculated. If so, it returns the cached valued, 
## otherwise it calculates the value of the inverse and sets the cache to this new value

cacheSolve <- function(x, ...) {                          ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()                               ## gets the cached value of the inverse matrix 
        if(!is.null(m)) {                                 ## if the value of the inverse matrix exists
                                                          ## it returns the cached value with the corresponding message
                message("getting cached data")
                return(m)
        }
        data <- x$getmatrix()                             ## if the inverse value does not exist, 
        m <- solve(data, ...)                             ## then it gets calculated
        x$setinverse(m)                                   ## and stored as a cached value
        m                                                 ## cacheSolve returns the inverse of the matrix - whether
                                                          ## newly calculated or cached
}


