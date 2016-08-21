
##In this example we introduce the `<<-` operator which can be used to
#assign a value to an object in an environment that is different from the
#current environment. Below are two functions that are used to create a
#special object that stores a matrix vector and caches its inverse matrix

##The first function, `makeCacheMatrix` creates a special "matrix", which is
#really a list containing a function to
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse matrix
#4.  get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
       
                im <- NULL
                set <- function(y) {
                        x <<- y
                        im <<- NULL
                }
                get <- function() x
                setimatrix <- function(imatrix) im <<- imatrix
                getimatrix <- function() im
                list(set = set, get = get,
                     setimatrix = setimatrix,
                     getimatrix = getimatrix)
       
}


## The following function calculates the inverse matrix of the special "matrix"
#created with the above function. However, it first checks to see if the
#inverse matrix has already been calculated. If so, it `get`s the inverse matrix from the
#cache and skips the computation. Otherwise, it calculates the inverse matrix of
#the data and sets the value of the inverse matrix in the cache via the `setimatrix`
#function.

cacheSolve <- function(x, ...) {
        im <- x$getimatrix()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setimatrix(im)
        im
}
