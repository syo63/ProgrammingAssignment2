## syo63
## Programming Assingment #2
## R Programming Class

##This is a pair of functions that calculates the inverse of a matrix. The value is then cached into memory.
##When a new matrix is input, recaluclate the inverse. Otherwise retrieve the cached value. 

## makeCacheMatrix is a function that creates a matrix object that can cache its inverse.
## Returns a list containing four functions.
## 1. set - set the value of the matrix
## 2. get - get the value of the matrix
## 3. setinverse - set the value of the inverse matrix
## 4. getinverse - get the value of the inverse matrix

makeCacheMatrix<- function(x=matrix()){
        ##Initialize variable m and define set function
        m<-NULL
        set<-function(y){
                 x<<-y
                 ## when new matrix is set, make m NULL. 
                 ##This is what is checked in cacheSolve to see if there has been a new matrix entered
                m<<-NULL   
        }
        
        ##initialize get, setinverse, and getinverse functions
        get<-function() x
        setinverse<- function(solve) m<<-solve
        getinverse<-function() m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ## return values
}

## cacheSolve computes the inverse of the matrix returned by mackeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## check to see if the inverse has already been calculated. 
        ##If it returns a value, the matrix inputs have not changed. 
        ##If it returns NULL, new matrix input and inverse has not been calculated. 
        m<-x$getinverse()    
        
        if((!is.null(m))){
        ## do not calculate, return value in memory
        message("getting cached data")
        return(m)
        } ## end if (!is.null(m))
        ## calculate and save inverse
        data <- x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m 
}
