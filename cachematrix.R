# based on the assignment examples

# this creates the matrix list
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # initiate an empty Inverse
        set <- function(y) { # function to set the matrix and cache it
                x <<- y
                inv <<- NULL
        }
        get <- function() x # get matrix
        setinv <- function(solve) inv <<- solve # create the inverse
        getinv <- function() inv # get the inverse
        list(set = set, get = get, # output a list of the things we just defined
             setinv = setinv,
             getinv = getinv)
}

# create the inverse - new or from cache
cacheSolve <- function(x, ...) {
        inv <- x$getinv() # get the inverse
        if(!is.null(inv)) { # check if inverse has been created before
                message("getting cached data")
                return(inv) # return cached inverse
        }
        data <- x$get() # get the matrix
        inv <- solve(data, ...) # invert the matrix
        x$setinv(inv) # set the matrix in makeCacheMatrix variable
        inv # output inv
}
