# this function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x= matrix()) {   
m <- NULL  #Inizialization of x and m  
set <- function(y) {    #set takes an argument named y     
 x <<- y   #assign the input argument to the x object   
 m <<- NULL #Assign the value of NULL to the m object in the parent environment    
}  
get <- function() x # R retrives x from the parent environment of makeCacheMatrix  
setinverse <- function(solve) m <<- solve  
getinverse <- function() m  
 list( set=set, get=get, setinverse=setinverse, getinverse=getinverse)
 }
        
# Returns a matrix that is the inverse of x, if it is in the memory, it will be "pulled" from there. 
# if not... it will be calculated 
cachesolve <- function(x,...){ 
m <- x$getinverse()  
if (!is.null(m)) { # this is done when the matrix is already in the memory   
message("getting cached data")    
return(m) 
}    
data <- x$get() 
m <- solve(data,...) #this is done when the matrix is new data 
x$setinverse(m)  
m
}
