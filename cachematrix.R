## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## updated 02/21/2016

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        make_matrix <- function(y){
                x <<- y
                inv <<- NULL
        }
        get_matrix <- function() x
        make_inverse <- function(inverse) inv <<- inverse
        get_inverse <- function() inv
        list( make_matrix = make_matrix,
              get_matrix = get_matrix,
              make_inverse = make_inverse,
              get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inverse()
        if(!is.null(inv)){
                message ("getting cached inverse of a matrix")
                return(inv)
        }
        data <- x$get_matrix()
        inv <- solve(data,...)
        x$make_inverse(inv)
        inv
}
