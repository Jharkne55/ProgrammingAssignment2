## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix consists of set, get, setinv, getinv
##library(MASS) is used to calculate inverse for nonsquared and squared matrices
library(MASS)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #this will start the operation of the inverse as NULL
  set <- function(y){
                    x <<-y
                    inv<<-NULL
                    }
  get <- function()x       #creating the function to return matrix x
  setinv <- function(inverse) inv<<-inverse
  getinv <- function(){
                       inver<- ginv(x)
                       inver%*%x    #creating the function to return the inverse matrix
                      }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
##The following will be used to get cache data

cacheSolve <- function(x, ...) #This return cache data
  {
  inv <-x$getinv()
  if(!is.null(inv)){          #this should check whether the inverse is NULL    
                    message("getting cached data!")
                    return(inv)   #returns inverse     
  }
  data <-x$get()
  inv<-solve(data,...)    #calculates the inverse value
  x$setinv(inv)
  inv              ## Return a matrix that is the inverse of 'x'
}

f <- makeCacheMatrix(matrix(1:10,2,5))
f$get()
f$getinv()
cacheSolve(f)
