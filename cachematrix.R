#the following two functions, makeCacheMatrix and cacheSolve, will allow you to cache matricies in the global environment, and 
#then access them to obtain the inverse of the matrix in the current environment. 

makeCacheMatrix <- function(X = matrix()) {
    #set inverse to empty for now
    Xi <- NULL
    #set inverse to empty and set matrix as Y in other environment, i.e. cache
    set <- function(y) {
        X <<- Y
        Xi <<- NULL
    }
    #get the inputed matrix
    get <- function() X
    #solve gets the inverse of of the function, and for the next function you want to be able to invoke setinv
    setinv <- function(solve) Xi <<- solve
    #you can then get this inverse with getinv, 
    getinv <- function() Xi
    #this is a list of the final functions that get invoked with makeCacheMatrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


#cacheSolve             

cacheSolve <- function(X, ...) {
    #you want to get inverse (R knows getinv from makeCacheMatrix) and R first tries to find it in cache, and tells user it is doing so
    #R then returns the inverse if it found it    
    Xi <- X$getinv()
    if(!is.null(Xi)) {
        message("getting cached data")
        return(Xi)
    }
    #if the inverse wasn't cached, R solves for the inverse, and then sets it as the inverse (setinv), then returns it
    data <- X$get()
    Xi <- solve(data, ...)
    X$setinv(Xi)
    Xi
}

#first run makeCacheMatirx(your_matrix_of_interest) and assign it to a variable (say C)
#then run cacheSolve(C)
#you can test it by just directly running solve(your_matrix_of_interest), which will give you the inverse without cacheing