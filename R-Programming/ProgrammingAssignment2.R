
makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    
    set = function(m)
    {
      x <<- m
      inv <<- NULL
    }
    
    get = function() {x}
    setInverse = function(inverse) {inv <<- inverse}
    getInverse = function() {inv}
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


cacheSolve <- function(x, ...) {      
   
   inv = x$getInverse()
   if(!is.null(inv))
   {
     message("getting cached data")
     return(inv)
   }
   
   m = x$get()   
   inv = solve(m)
   x$setInverse(inv)
   inv 
}



sqMtx = matrix(sample.int(15, 10*10, TRUE), 10, 10)


c = makeCacheMatrix(sqMtx)


cacheSolve(c)
