## function : makeCacheMatrix  
## Description  : This function creates a special "matrix", 
## which is really a list containing a function to
## 1- set the value of the vector
## 2- get the value of the vector
## 3- set the value of the inverse matrix
## 4- get the value of the inverse matrix
## Note : it is assumed that the matrix supplied is always invertible
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversematix <- function(solve) m <<- solve
        getinversematix <- function() m
        list(set = set, get = get,
             setinversematix = setinversematix,
             getinversematix = getinversematix)
}

## function : cacheSolve  
## Description  : This function allows to calculate the inverse 
## of the matrix, given in input parameter 'x'
## The function checks if the inverse has already been calculated
## if yes, it 'get' the inverse from the cache.
## otherwise, it calculates the inverse of the data 
## and sets the value of the inverse in the cache 
## via the 'setinversematix' function 
cacheSolve <- function(x) {
		## Return a matrix that is the inverse of 'x'
		m <- x$getinversematix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinversematix(m)
        m
}
