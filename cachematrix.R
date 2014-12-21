## Put comments here that give an overall description of what your
## functions do

## this function is similar to "makeVector" from the assignment specifications
## inv is initialized to a square matrix with NA's as elements

makeCacheMatrix <- function(x = matrix()) {
    nel=min(nrow(x), ncol(x))
    inv=matrix(rep(NA, nel*nel), nel, nel)
    set=function(y){
        x <<- y
        inv <<- matrix(rep(NA, nel*nel), nel, nel)
    }
    get=function() x
    setinv=function(y) inv <<- y
    getinv=function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## the principle in this function is similar to "cachemean" from the assignment specifications
## for the if's condition, only the first element of the matrix is tested against NA
## when solve(a) fails inv is not changed and the message "NA: non-invertible matrix" is generated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv=x$getinv()
    if(!is.na(inv[1,1])){
        return(inv)
    }
    data=x$get()
    try(inv <- solve(data), silent=TRUE)
    if(is.na(inv[1,1])) {
        print("NA: non-invertible matrix")
        return(inv)
    }
    x$setinv(inv)
    inv
}
