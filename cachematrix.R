# makeCacheMatrix will create an environment to store the matrix and its inverse matrix
# it will return a list of functions like APIs, you can set/get the matrix, and set/get the 
# inverse matrix. The inverse of the specified matrix will be set to null while the data is set 
# by the function set

makeCacheMatrix<-function(x=matrix()){
	matrix_inverse<-NULL;
	set<-function(y){
		x<<-y;
		matrix_inverse<-NULL;
	}
	get<-function()x;
	setInverse <- function(inverse){
		matrix_inverse<<-inverse;
	}
	getInverse<-function() matrix_inverse;
	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse);
}



# cacheSolve function accepts the environment created by makeCacheMatrix, it firstly
# check whether the inverse is null, if it is, then calculate the inverse matrix using solve 
# function, or it will directly return the cached inverse matrix which has been calculated 
# before


cacheSolve<-function(x,...){
	inverse<-x$getInverse();
	if(!is.null(inverse)){
		message("getting the inverse from the cached data");
		return(inverse);
	}
	data<-x$get();
	inverse<-solve(data,...);
	x$setInverse(inverse);
	return(inverse);
}
        ## Return a matrix that is the inverse of 'x'

