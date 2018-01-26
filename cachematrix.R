## Week 3 Programming Assignment of the Coursera Intro to R Course.
## Week beginning, 21 January 2018. 
## Submitted by GitHub User: igsaisb

## This pair of functions to calculate and cache a linear algegra matrix inversion.
## All matrices assumed to be invertible as given in assigment.
## The mackeCacheMatrix function creates a placeholder matrix object to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {   ## create a function with default matrix object x as argument
        inversion <- NULL                     ## variable named inversion, a placeholder for the matrix inversion, initialized as NULL             
        set <- function(y){                   ## set function defined; sets new matrix value 
                x <<- y                       ## assinging the value of new matrix to parent environment
                inversion <<- NULL            ## reset inversion to NULL in event of new matrix
        }
        get <- function() x                   ## get function defined, get returns matrix 
        
        setInversion <- function(solveMatrix) inversion <<- solveMatrix ##assigns value of inversion in parent environment
        
        getInversion<- function() inversion  ##retrieves value of inversion when called
        
        list(set = set, get = get, setInversion = setInversion, getInversion = getInversion)
        ##naming a list whose elements are the function names allows for use of $ operator to use those functions
}


## This function returns previously calculated matrix inversion from cache or 
## computes the inversion of new matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {            ## Returns inversion of matrix x
        
        inversion <- x$getInversion()       ##function tries to retrieve matrix inversion for x from cache
        if(!is.null(inversion)){            ## tests if matrix inversion of x already exists in cache
                message("retrieving cached data") ##if inversion of x already cached, lets user know 
                return(inversion)                 ## returns cached matrix inversion to parent environment
        }
        targetMatrix <- x$get()             ##if no inversion for x cached, function gets the new matrix object x
        inversion <- solve(targetMatrix)    ##calculates matrix inversion for x and assigns to inversion variable
        x$setInversion(inversion)           ## return newly calculated inversion value to parent environment
        inversion                           ## print matrix inversion of x              
}
