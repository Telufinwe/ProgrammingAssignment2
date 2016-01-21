## Starting from a normal square (and invertible) matrix, the two functions combined allow to
## build a special matrix, calculate the inverse of the matrix and check if the inverse has already
## been calculated, in this way avoiding to waste time in repeating unnecessary calculations.

## The first function takes a normal square numeric matrix and transform
## it to a special matrix provided with in-built functions that allow to:
## 1- modify the value of the matrix.
## 2- retrieve the matrix.
## 3- set the inverse of the matrix to a given matrix (it does not calculate the inverse).
## 4- retrieve the inverse of the matrix if it has already been calculated.

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y) {   ## (1)
            x <<- y
            invrs <<- NULL
        }
        get <- function() {   ## (2)
		    x
        }			
        setinverse <- function(somematrix) {   ##(3)
		    invrs <<- somematrix
        }			
        getinverse <- function() {   ## (4)
		    invrs 
        }	 		
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        ##  the special matrix consists of a list of functions 
}


## The second function takes a special matrix, built using the first 
## function, and checks if its inverse has already been calculated:
## 1- if it has, the function returns the inverse without calculating it again.
## 2- if it has not, the function calculates the inverse and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		invrs <- x$getinverse()
        if(!is.null(invrs)) {   ## check
                message("getting cached data")    ## case (1)
                return(invrs) 
        }
        originalmatrix <- x$get()     ## case (2)
        invrs <- solve(originalmatrix, ...)   
        x$setinverse(invrs)
        invrs
}
