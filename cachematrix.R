## A couple of functions which cache the inverse of a matrix

## Here we create a special matrix

makeCacheMatrix <- function(x = matrix()) {
##Initializing the inverse 

  m<-NULL
  
##Set the matrix

  set<-function(y){
  x<<-y
  m<<-NULL
}

##Retrieve the matrix by returning x
get<-function() x

##Inverse the matrix
setmatrix<-function(solve) m<<- solve

##Retrieve the inverse of the matrix
getmatrix<-function() m

##Return a list
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

## Compute the inverse of the matrix which is returned by makeCacheMatrix.
##First checks if it has been calculated. if yes, it doesn't recalculate it, just gets the result.
##If not, then it computes the inverse and sets it value

cacheSolve <- function(x, ...) {
         m<-x$getmatrix()
         
##Checking if inverse is already set or not
         
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }

##Get the matrix
    matrix<-x$get()
    
##Compute the inverse
    m<-solve(matrix, ...)
    
##Set the inverse
    x$setmatrix(m)
    
    m   ## Return a matrix that is the inverse of 'x'
}
