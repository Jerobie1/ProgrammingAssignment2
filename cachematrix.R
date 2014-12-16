## cachematrix.r has two primary functions makeCacheMatrix and cachesolve
##
##Together these functions provide for a one time matrix inversion (solve) with results 
##and the ability to subsequently retrieve results without requiring to re-execute a solve.


## makeCacheMatrix is a function of functions establishing a link between three functions, 
## "get", "setsolve" and "getsolve", and a provided matrix.. 

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL                               # clear object "s"
    get <- function() x                     # define "get"function
    setsolve <- function(solve) s <<- solve # define "setsolve" function
    getsolve <- function() s                # define "getsolve" function
    list(get = get,                         # provide list of the available/associated functions
         setsolve = setsolve,
         getsolve = getsolve)
}


##cachesolve makes use of the makeChcheMatrix functions to  calculate and cache the 
##matrix inversion results and subsequently retrieve the cached results, in lieu of recalculation. 

cachesolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()                      # call getsolve to load cache of solve(data) of "x" - "s"
    if(!is.null(s)) {                      # if cache solve(data) of "x" exist use cache
        message("getting cached data")     # and display/return the results
        return(s)
    }
    data <- x$get()                        # if not - - store the associated matrix in data
    s <- solve(data, ...)                  # solve for inverse matrix
    x$setsolve(s)                          # cache the solve
    s                                      # return the result
}