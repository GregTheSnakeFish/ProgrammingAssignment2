
makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x<<- y
        inver <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inver <<- inverse
    getinverse <- function() inver
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inver <- x$getinverse()
    if (!is.null(inver)){
        message("getting cached data")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data, ...)
    x$setinverse(inver)
    inver
}

#Below is the code for a nifty way to test one's work that I found at 
#https://masterr.org/r/how-to-cache-a-matrix-inversion-in-r/

# test = function(mat){
#   ## @mat: an invertible matrix
#   
#   temp = makeCacheMatrix(mat)
#   
#   start.time = Sys.time()
#   cacheSolve(temp)
#   dur = Sys.time() - start.time
#   print(dur)
#   
#   start.time = Sys.time()
#   cacheSolve(temp)
#   dur = Sys.time() - start.time
#   print(dur)
# }
