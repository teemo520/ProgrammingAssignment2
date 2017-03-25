## This function creates a special "matrix" (list) object
# that can cache its inverse.


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



#＃ The following function calculates his function computes the inverse of the special
#＃ "matrix" (list) returned by `makeCacheMatrix` above. If the inverse has
#＃ already been calculated (and the matrix has not changed), then
#＃ `cacheSolve` should retrieve the inverse from the cache.


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

