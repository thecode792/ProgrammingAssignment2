## Put comments here that give an overall description of what your
## functions do
##Working together, these functions allow someone to find the inverse of a matrix efficiently. The first function provides the second with all the information it needs to get the information on the matrix values (includoing dimensions and what data is inside the matrix itself). Moreover, the first function has room to store a 'cache' of the inverse of the matrix after it has already been computed within it's own environment. The advantage of this is that since the values are not stored in the global environment, there is no risk that the value will accidentily be reset or changed. The second function either gets the value of the cached matrix if it has already been computed (which saves time since the computation doesn't have to be done every time), or if the value is not stored, it will compute it within the second function using the data about the matrix stored in the first function and then output the result, caching it for future use in the process.
## Write a short comment describing this function

##This function is given what a matrix would be given as an input. Using that data, it outputs a list of four items. The first is an item that allows someone to reset the value of the matrix by subsetting the get output on the list, the second allows someone to retreive the matrix data that is stored in the makeCacheMatrix environment by subsetting out the get part of the list, the third item allows you to manually set the inverse of the matrix within makeCacheMatrix by subsetting out the setinverse part of the list, and the fourth item allows you to retreive the value of the inverse manually by subsetting out the getinverse item of the list. This function utilizes lexical scoping: for example, in the set part of the function, if you manually set the values of the matrix to something different, the inverse is reset to null and thus the cached inverse is erased (because a new set of matrix data will likely have a new inverse).

makeCacheMatrix <- function(x, row, col) {



##Set the value of the matrix
     i <- NULL
        set <- function(y, yrow, ycol) { 
                     x <<- y
                     row <<- yrow
                     col <<- ycol
                     i <<- NULL
      }



##Get the value of the Matrix
      get <- function() matrix(x, row, col)


##Set the value of the Inverse
      setinverse <- function(inverse) i <<- inverse


##Get the value of the inverse
      getinverse <- function() i



##List everything so that it can be used by cacheSolve
        list(set = set, get = get,
                setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

##Computes the inverse of a matrix returned by makecachematrix. If the inverse has already been calculated, then the cachesolve should retreive the inverse from the cache, which is part of the makecachematrix function, and return message "getting cached data".

## 1. Create an object cacheSolve that is a function with input in form of list like in makeCacheMatrix

cacheSolve <- function(x, ...) {

##2. create an object that pulls the inverse from the input
        
        i <- x$getinverse()
        

##3. Check if the inverse exists, and if it does return a message getting cached data and reuturn the inverse
        if(!is.null(i)) { 
                message("getting cached data")
                return(i)
        }

##4. otherwise, create an object that stores the matrix
        
        data <- x$get()

##5. Create an object that will compute the inverse of the object just created

        i <- solve(data, ...)
        
##6. store the inverse into the cache of the original fn by calling the setinverse
        
        x$setinverse(i)

##7. return the inverse 
        
        i
}
