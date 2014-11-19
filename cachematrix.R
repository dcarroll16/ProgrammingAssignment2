## These functions create an ojbect that can store a matrix, calculate its inverse
## and store its inverse once calculated.


## makeCacheMatrix creates the oject to store the matrix and its inverse
## It also creates a list of functions to set and call these variables.
makeCacheMatrix <- function(cached_Mtx = matrix()) {
        inv_Mtx <- NULL #sets the inital value of the inverse to NULL 
        
        #following functions store and retrieve the matrix and its inverse
        set <- function (temp_Mtx) { 
                cached_Mtx <<- temp_Mtx
                inv_Mtx <<- NULL
        }
        get <- function() cached_Mtx 
        set_inv <- function(temp_inv_Mtx) inv_Mtx <<- temp_inv_Mtx 
        get_inv <- function() inv_Mtx 
        
        #creates a list of functions so they can be called from outside of this function
        list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## cacheSolve checks takes an object created from makeCacheMatrix and
##      1) returns a previously calculated inverse if it exists
##      2) or calculates and returns the inverse if it does not exsist
cacheSolve <- function(x, ...) {
        
        #check to see if the inverse matrix was already calculated and stored
        inv_Mtx <- x$get_inv()
        if(!is.null(inv_Mtx)) {
                message("getting cached inverse matrix")
                return(inv_Mtx)        #return inverse matrix if it exists and exits function
        }
        
        #otherwise, calculates and stores the inverse matrix
        cached_Mtx <- x$get()
        inv_Mtx <- solve(cached_Mtx)
        x$set_inv(inv_Mtx)
        inv_Mtx
}