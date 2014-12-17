###Test matrix that can be used to test the program

test_M <- function(n){
     # n <- 5
      v_mx <- sample(1:(10*n), n^2, replace = TRUE) #creates a randome database
      bm <- matrix(v_mx, nrow=n, ncol=n)

      # before calling, assign to variable
      
}
      
makeCacheMatrix <- function(x=matrix()){
      
#Creating a list of functions
      #* Matrix + Inverse
      #* Matrix
      #* Calculated Inverse
      #* Value of calculated inverse
      
      #These functions arrange the logic of the cachesolve programe so that it 
      #reduces the need to write variables in the general memory. It basically
      # creates the function's internal RAM with a list

      
      # The first part just setts the value of Inverse to 0 - empty
      Invr <- NULL
      
      set <- function(y){
            
            x <<- y
            Invr<<- NULL
            
      }
      #The set function will assign the inverse to the vector as the 1st element
      
      #Indicating the matrix at hand to be used in the cachesolve function      
      get <- function(){x}
      
      #This functionwill set the inverse when calculated
      setInvr <- function(solve) {Invr <<- solve}
      
      #It basically just says that you will get the inverse when calling the f.
      getInvr <- function() {Invr}
      
      list(set =set, get=get, setInvr= setInvr, getInvr=getInvr)
      # we name the elements by their variable names so that we can refer to 
      # them in cachesolve with $
            
}
      
# The function that actually looks for the solution

cachesolve <- function(x, ...) {
      
      Invr <- x$getInvr()
      #It looks for the written value of Inverse if the x matches to the Matrix
      #for which the inverse was already calculated
      
      if(!is.null(Invr)){
            message("getting cached data")
            return(Invr)
      }
      #We make sure that we indicate thatwe have found it +display it directly


      data <- x$get()   #It looks for the matrix
      Invr <- solve(data, ...)      #It calculates the inverse
      x$setInvr(Invr)   #it sets an inverse
  
      # Printing the solution
      Invr  
      
}
