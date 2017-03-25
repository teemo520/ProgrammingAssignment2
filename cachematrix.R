## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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



## Write a short comment describing this function

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

