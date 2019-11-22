realEstateDictionary <- function(XVARS,TEXTVAR,YVAR,DATA)
{
require(slam)
require(SparseM)
require(Matrix)
require(glmnet)
require(tm)
X1 <- as.matrix(DATA[,colnames(DATA)%in%XVARS])
TEXT0 <- DATA[,which(colnames(DATA)==TEXTVAR)]
Y1 <- DATA[,which(colnames(DATA)==YVAR)]
TEXT1 <- cleanText(TEXT0 , removeStopWords = FALSE)
M1 <- tokenMatrixMaker(TEXT1)
FIT <- lassoPostLasso(X1,M1,Y1)
return(FIT)
}
