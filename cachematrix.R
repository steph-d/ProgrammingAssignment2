## Functions to caching the inverse of a matrix and then 
## call it in order not to calculte it again

## Function 1: caching the inverse of a matrix
## this function returns a list of 4 elements

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y = matrix()) {
          x <<- y # in the environment of this function, we fix the values of x
          m <<- NULL ## in the environment of this function, we fix the values of m
     }
     get <- function() x # to get the value of the matrix
     setsolve <- function(solve) m <<- solve # to set the value of the inverse of the matrix
     getsolve <- function() m # to get the value of the inverse of the matrix
     list(set = set, get = get, setsolve = setsolve, getsolve = getsolve) # the return list
}

# for example, we can define the matrix x
x <- matrix(1:4,2,2)
x
h<-makeCacheMatrix(x)
h # we can see the 4 elements of the list and the enrironment where it is stored

# ------------------------------------------------------------
## Function 2: Retrieve the inverse of a matrix if it exists in cache

cacheSolve <- function(x, ...) {
        m <- x$getsolve() # we call the element "getsolve" of the list returned in the Function 1
        if(!is.null(m)) { # if m is not null, so we have a value in cache and we call it
             message("getting cached data")
             return(m)
        }
        data <- x$get() # if there is no value in cache, we calculate it and return it
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

# for example, we can call the function on the result from Function 1
hh<-cacheSolve(h)
hh ## we obtain the inverse of the matrix x
hh<-cacheSolve(h) # if we repeat the command, we add the message "getting cached data"
# because the value was yet in cache

