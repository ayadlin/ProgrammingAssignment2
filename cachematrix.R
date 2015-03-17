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

## Applications:
> source('C:/Users/Ale/Dropbox (Yadlin Family)/Coursera/2 R Programing/Programing Assignment 2/makeCacheMatrix.R')
> source('C:/Users/Ale/Dropbox (Yadlin Family)/Coursera/2 R Programing/Programing Assignment 2/cacheSolve.R')

> m<-makeCacheMatrix(magic(5)) ##--> magic produces square invertible matrices 
> m1<-m$getMatrix()

> m2<-m$setInverse(matrix( c(1:35), nrow=7, ncol=5))
the Inverse matrix you have input has the wrong dimensions

> m2<-m$setInverse(diag(5))
the inverse matrix you have input is wrong try using m$setInverse(solve(m$getMatrix()))

> m2
NULL

> cacheSolve(m)
calculating inverse matrix and saving in cache memory
             [,1]         [,2]         [,3]         [,4]         [,5]
[1,]  0.011089744 -0.045000000  0.041538462  0.005000000  0.002756410
[2,] -0.036987179  0.043461538  0.010769231 -0.006538462  0.004679487
[3,]  0.036410256  0.003076923  0.003076923  0.003076923 -0.030256410
[4,]  0.001474359  0.012692308 -0.004615385 -0.037307692  0.043141026
[5,]  0.003397436  0.001153846 -0.035384615  0.051153846 -0.004935897

> cacheSolve(m)
getting inverse matrix from cache
             [,1]         [,2]         [,3]         [,4]         [,5]
[1,]  0.011089744 -0.045000000  0.041538462  0.005000000  0.002756410
[2,] -0.036987179  0.043461538  0.010769231 -0.006538462  0.004679487
[3,]  0.036410256  0.003076923  0.003076923  0.003076923 -0.030256410
[4,]  0.001474359  0.012692308 -0.004615385 -0.037307692  0.043141026
[5,]  0.003397436  0.001153846 -0.035384615  0.051153846 -0.004935897

> m2<-cacheSolve(m)
getting inverse matrix from cache

> m2<-m$setInverse(diag(5))
the Inverse matrix value in the cache memory is already correct - no changes will be accepted
you can use x$getInverse to retrieve the correct Inverse matrix value from the Cache
if you were using x$setInverse(matrix) the correct value will appear below this message
if you were using M<-x$setInverse(matrix), the correct value will be stored in 'Matrix'

> m$setInverse(diag(5))
the Inverse matrix value in the cache memory is already correct - no changes will be accepted
you can use x$getInverse to retrieve the correct Inverse matrix value from the Cache
if you were using x$setInverse(matrix) the correct value will appear below this message
if you were using M<-x$setInverse(matrix), the correct value will be stored in 'Matrix'
             [,1]         [,2]         [,3]         [,4]         [,5]
[1,]  0.011089744 -0.045000000  0.041538462  0.005000000  0.002756410
[2,] -0.036987179  0.043461538  0.010769231 -0.006538462  0.004679487
[3,]  0.036410256  0.003076923  0.003076923  0.003076923 -0.030256410
[4,]  0.001474359  0.012692308 -0.004615385 -0.037307692  0.043141026
[5,]  0.003397436  0.001153846 -0.035384615  0.051153846 -0.004935897

> m2
             [,1]         [,2]         [,3]         [,4]         [,5]
[1,]  0.011089744 -0.045000000  0.041538462  0.005000000  0.002756410
[2,] -0.036987179  0.043461538  0.010769231 -0.006538462  0.004679487
[3,]  0.036410256  0.003076923  0.003076923  0.003076923 -0.030256410
[4,]  0.001474359  0.012692308 -0.004615385 -0.037307692  0.043141026
[5,]  0.003397436  0.001153846 -0.035384615  0.051153846 -0.004935897

> m$getMatrix()
     [,1] [,2] [,3] [,4] [,5]
[1,]    9    2   25   18   11
[2,]    3   21   19   12   10
[3,]   22   20   13    6    4
[4,]   16   14    7    5   23
[5,]   15    8    1   24   17

> m$getInverse()
             [,1]         [,2]         [,3]         [,4]         [,5]
[1,]  0.011089744 -0.045000000  0.041538462  0.005000000  0.002756410
[2,] -0.036987179  0.043461538  0.010769231 -0.006538462  0.004679487
[3,]  0.036410256  0.003076923  0.003076923  0.003076923 -0.030256410
[4,]  0.001474359  0.012692308 -0.004615385 -0.037307692  0.043141026
[5,]  0.003397436  0.001153846 -0.035384615  0.051153846 -0.004935897

> m$getMatrix()%*%m$getInverse()
              [,1]         [,2]          [,3] [,4]          [,5]
[1,]  1.000000e+00 7.979728e-17 -1.110223e-16    0 -6.938894e-18
[2,]  3.469447e-17 1.000000e+00  1.110223e-16    0 -7.632783e-17
[3,] -2.428613e-17 4.943962e-17  1.000000e+00    0  3.469447e-18
[4,]  5.551115e-17 1.318390e-16  1.110223e-16    1  0.000000e+00
[5,]  8.326673e-17 1.838807e-16 -3.330669e-16    0  1.000000e+00

> round(m$getMatrix()%*%m$getInverse())
     [,1] [,2] [,3] [,4] [,5]
[1,]    1    0    0    0    0
[2,]    0    1    0    0    0
[3,]    0    0    1    0    0
[4,]    0    0    0    1    0
[5,]    0    0    0    0    1

> round(m1%*%m2)
     [,1] [,2] [,3] [,4] [,5]
[1,]    1    0    0    0    0
[2,]    0    1    0    0    0
[3,]    0    0    1    0    0
[4,]    0    0    0    1    0
[5,]    0    0    0    0    1

> cacheSolve(m)
getting inverse matrix from cache
             [,1]         [,2]         [,3]         [,4]         [,5]
[1,]  0.011089744 -0.045000000  0.041538462  0.005000000  0.002756410
[2,] -0.036987179  0.043461538  0.010769231 -0.006538462  0.004679487
[3,]  0.036410256  0.003076923  0.003076923  0.003076923 -0.030256410
[4,]  0.001474359  0.012692308 -0.004615385 -0.037307692  0.043141026
[5,]  0.003397436  0.001153846 -0.035384615  0.051153846 -0.004935897

# hope this is clear to all readers
