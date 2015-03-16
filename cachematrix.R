## Coursera Programing Assignement 2, Program 1
## makeCacheMatrix:
## This function is able to create a matrix and store it and its inverse in Cache memory 
## as well as retrieve them as needed. It also keeps the correct Inverse matrix in the Cache if
## mistakenly replaced by an erroneous one by hand

makeCacheMatrix <- function(x = matrix()) {
  ## by default x is an empty matrix
  
  Inverse <- NULL
  ## by default the inverse matrix of x is set to NULL
  
  ## This function consists of 4 sub functions:
  
  #1: setMatrix --> set the value of matrix x to mat 
  setMatrix <- function(mat){
    x <<- mat
    Inverse <<- NULL
    ## Every time new matrix (x) is input using the set function, inverse is reset to NULL 
  }
  
  #2 getMatrix function simply returns the last input (mat) for matrix x from the Cache memory
  getMatrix <- function() x
  
  #3 setInverse assigns the value to Inverse (x's inverse matrix) and sotres it in the Cache memory
  ## - should be the inverse of x - any value can be assigned manually, I wrote it to ensure the inpur matrix  
  ## is at least square and has the same dim as X. However - wrong matrix could be input  and is 
  ## better to use x$setInverse(solve(x)) or cahceSolve(x) to get the correct value.  
  
  
  setInverse <- function(a)
    
    ## This will ensure that no incorrect values are accepted
    ## the values for x's inverse inthe cahce will be either its correct Inverse or Null
    if (!is.null(Inverse) && max(dim(Inverse)==max(dim(x))) && identical(round(x%*%Inverse),diag(max(dim(x))))){
    ## This stipulates that as long as Inverse is not NULL, its dimensions are same as x (assuming square matrix)
    ## and Inverse is actually x current Inverse matrix no changes will happen in the cache memory.
      
      message("the Inverse matrix value in the cache memory is already correct - no changes will be accepted")
      message("you can use x$getInverse to retrieve the correct Inverse matrix value from the Cache")
      message("if you were using x$setInverse(matrix) the correct value will appear below this message")
      message("if you were using M<-x$setInverse(matrix), the correct value will be stored in 'Matrix'")
      return(Inverse)
      
    } else {
    if (max(dim(a))!=max(dim(x))|max(dim(a)!=min(dim(a)))) {       
      Inverse<<- NULL;                                             #avoid wrong dimensions or non-square matrix
      message("the Inverse matrix you have input has the wrong dimensions")
    }else{
      if(identical(round(x%*%a),diag(max(dim(x))))) {
        Inverse <<- a                                              #if correct inverse matrix entered keep
      } else {
        Inverse<<- NULL;                                           # otherwise inverse still is null
        message("the inverse matrix you have input is wrong try using m$setInverse(solve(m$getMatrix()))")}        
    }
    }
  
  #4 getInverse returns the value of x inverse matrix
  getInverse <- function() Inverse
  
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
  ## creates a list of the 4 sub-functions needed to run the main makeCacheMatrix function
  
}


## Coursera Programing Assignement 2, Program 2
## cacheSolve: 
## This function calculates the Inverse of matrix x stored by makeCacheMatrix. 
## There are two ways this function can work:
#1 If the Inverse Matrix has been calculated and stored in the Cache memory it simply retieves it , 
#2 If the inverse matrix has not been calculated or a new matrix has been inputed in makeCacheMatrix 
## setting back Inverse to null cacheSolve will (re)calculate the Inverse Matrix and stored in the Cache memory.

cacheSolve <- function(x, ...) {
  
  Inverse <- x$getInverse()        ## Retrives the value of x Inverse matrix from the Cache memory
  
  
  if(!is.null(Inverse)){
    message("getting inverse matrix from cache")
    return(Inverse)                ## Return the stored Inverse value of x
  }
  
  ## If the Inverse matrix is NOT null it was previously calculated and stored in the Cache, 
  ## cacheSolve returns the stored value of x's Inverse
  ## the message: "getting inverse matrix from cache" is displayed
  
  
  matrix <- x$getMatrix()          ## Retrieves x from Cache                           
  Inverse <- solve(matrix, ...)    ## calculates x inverse
  message("calculating inverse matrix and saving in cache memory") 
  x$setInverse(Inverse)            ## Sets Inverse to the newly calculated value 
  return(Inverse)                  ## Return the calculated Inverse value of x
  
  ## If Inverse is NULL, because it wasn't previously calculated or x has changed setting Inverse back to null
  ## then (the new) matrix x is retrieved, its Inverse is calculated and and sored in the Cache memory
  ## the message: "calculating inverse matrix and saving in cache memory" is displayed
  ## the R function solve is used to calculate a matrix inverse
}
