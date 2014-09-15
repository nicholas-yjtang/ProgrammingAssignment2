## @author Nicholas Tang
## makeCacheMatrix will create a special object known
## that stores both the original matrix
## and the cached inverse matrix result
##
## Arguments
## @argument x a matrix to be converted into a cacheMatrix
##

makeCacheMatrix <- function(x = matrix()) {
    #the cached matrix
    s <- NULL

    #set function
    #set a new matrix into this object
    #sets the s back to NULL so that the result
    #can be recomputed again
    set <- function(y) {
          x <<- y
          s <<- NULL
    }
    
    #get function
    #gets the matrix that has been 
    #set into this object
    get <- function() x

    #setsolve function
    #stores a value (solved_inverse) into the variable s
    #as the cached inverse matrix result
    setsolve <- function(solved_inverse) s <<- solved_inverse

    #getsolve function
    #returns the cached inverse matrix result
    getsolve <- function() s
    list(set=set,
         get=get,
         setsolve = setsolve,
         getsolve = getsolve
         )
}


## cacheSolve will solve, given the special cachematrix type
## either by returning the cached value from the special cachematrix
## or to solve it and store the result into the special cachematrix cache
##
## @argument x the special cached matrix object that will be used to store cached 
## results of the inverse matrix operation

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        ## if the result is not null (meaning a cached result)
        ## we simply return it
        if (!is.null(s)) {
          message("getting cached data")
          return(s)
        }
        ## else we will go about solving the matrix
        ## get the original matrix first
        data <- x$get()
        ## solve the matrix and store the result into variable s
        s <- solve(data)
        ## set the solved result back into the cached matrix object x
        x$setsolve(s)
        s
}
