



## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y)
  {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inv) m<<-inv
  getinverse<-function() m
  list( set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  

}


## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed), then cacheSolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}



##source('E:/R/Data-Science/ProgrammingAssignment2/cachematrix.R')
##mat<-makeCacheMatrix(matrix(c(1,2,3,4),c(2,2)))
##cacheSolve(mat)

##       [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

