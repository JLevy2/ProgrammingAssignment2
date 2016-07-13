
# Part 1. `makeCacheMatrix`: This function creates a special "matrix" object
#    that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	#1. initialize an empty inverse matrix 
	m <- NULL
	
	#2. set the matrix. Remove any data that previously filed the matrix and create and new matrix by filling it
      # with a set of values defined by y
            set <- function(y) {
                    x <<- y
                    m <<- NULL
           }

	#3. get the matrix
	            get <- function() x
	#4.  set the inverse matrix
			setinverse<-function(inverse) m<<-inverse
	#5.  get the inverse matrix
			getinverse<-function()m
	#6. create a list that will be used as the input 
		#for the catchSolve function in part 2
			list(set=set,get=get,
				setinverse=setinverse,
				getinverse=getinverse)
}

#__________________________________________________________________________________________
#Part 2

#cacheSolve`: This function computes the inverse of the special
    "matrix" returned by `makeCacheMatrix` above. If the inverse has
    already been calculated (and the matrix has not changed), then
    `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        #1. Return a matrix that is the inverse of 'x'
 		m <- x$getinverse()
 	  #2. If the inverse has a value(is not missing) use the cache value
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
	#3. If the inverse was not already calculated, get the data and use the solve function
	#to calculate the inverse 
            data <- x$get()
            m <- solve(data, ...)
	#4. set the value of inverse from step 4 in part 1 
            x$setinverse(m)
	#return the value of the inverse
            m
}

#Check that the function works
# create a 2x2 matrix
a <- matrix(data = c(3,7,9,10), nrow = 2, ncol = 2)
a
#Create a special "matrix" object that can cache its inverse.
b <- makeCacheMatrix(a)
#solve the inverse of the matrix
cacheSolve(b)

#__________________________________________________________________________________________
#Check that the inverse was calculated correctly

#The inverse should 1)swap the positions of 3 and 10, 2) put negatives in front of 7 and 9, 3) and divide everything by the determinant (3*10-7*9) = -33.

#Divide Everything by the determinent (#3)
a/-33

# Visually check that the positions that positions 3+10 have been swapped(#1), positions where 7 & 9 were located are now postiive (#2),
 and that the nubers align with the previous step
cacheSolve(b)

# LOOKS GOOD
