postLasso <- function(XXX,YYY)
{
require(slam)
require(SparseM)
require(Matrix)
if(class(XXX)[1]!="dgCMatrix") XXX <- as(XXX, "sparseMatrix")
  	COLMAX <- colapply_simple_triplet_matrix( as.simple_triplet_matrix(XXX) , max )
	COLMIN <- colapply_simple_triplet_matrix( as.simple_triplet_matrix(XXX) , min )
  	if(min(COLMAX-COLMIN)!=0) XXX <- cBind(XXX,1)
	XCSC <- new("matrix.csc", ra = XXX@x,
              ja = XXX@i + 1L,
              ia = XXX@p + 1L,
              dimension = XXX@Dim)
	XCSR <- as.matrix.csr(XCSC)
	slmFit <- slm.fit(XCSR,YYY,tmpmax=1000*nrow(XXX))
return(slmFit)
}
