###Test matrix that can be used to test the program

test_M <- function(n){
     # n <- 5
      v_mx <- sample(1:(10*n), n^2, replace = TRUE)
      bm <- matrix(v_mx, nrow=n, ncol=n)
      x <- bm
      
}

### Assignment
      
makeCacheMatrix <- function(x=matrix()){
      
#Creating a list of matrix value and the inverse belonging to the matrix
      
      Invr <- NULL
      set <- function(y){
            
            x <<- y
            Invr<<- NULL
            
      }
      
      get <- function(){x}
      setInvr <- function(solve) {Invr <<- solve}
      getInvr <- function() {Invr}
      list(set =set, get=get, setInvr= setInvr, getInvr=getInvr)
      
            
}
      
# The function that actually looks for the solution

cachesolve <- function(x, ...) {
      
      Invr <- x$getInvr()
      if(!is.null(Invr)){
            message("getting cached data")
            return(Invr)
      }
      
      data <- x$get()
      Invr <- solve(data, ...)
      x$setInvr(Invr)
  
      # Printing the solution
      
      Invr  
      
}
