## Take the inverse of an inversible matrix.
## If the contents of a matrix are not changing, cache the value of the
## inverse so that when we need it again, it can be looked up in the 
## cache rather than recomputed.

## Function makeCacheMatrix creates a matrix, finds its inverse
## set & get the value of the matrix
## set & get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y) {
                x<<-y
                i<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) i<<-inverse
        getinverse<-function() i
        list(set=set, get=get, setinverse=setinverse,
             getinverse=getinverse)
}


## function cacheSolve calculates the inverse of the special "matrix"
## created with the makeCacheMatrix function.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        i<-x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data)
        x$setinverse(i)
        i
}
