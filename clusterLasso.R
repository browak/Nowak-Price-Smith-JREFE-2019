clusterLasso <- function(XXX,YYY,QQQ=NULL,CCC=NULL,GGG=NULL,TARGETVARS=NULL,ALPHA=NULL)
{
  require(glmnet)
  require(SparseM)
  require(Matrix)
  require(slam)
  blanks <- which(colnames(XXX)=="")
  if(0<length(blanks))  colnames(XXX)[blanks] <- paste0("nonTokenPredictor",1:length(blanks))
  if(class(XXX)[1]!="dgCMatrix") XXX <- as(XXX, "sparseMatrix")
  COLMAX <- colapply_simple_triplet_matrix( as.simple_triplet_matrix(XXX) , max )
  COLMIN <- colapply_simple_triplet_matrix( as.simple_triplet_matrix(XXX) , min )
  if(min(COLMAX-COLMIN)!=0) XXX <- cBind(XXX,1)
  colnames(XXX)[ncol(XXX)] <- "intercept"
  XCSC <- new("matrix.csc", ra = XXX@x,
              ja = XXX@i + 1L,
              ia = XXX@p + 1L,
              dimension = XXX@Dim)
  XCSR <- as.matrix.csr(XCSC)
  XXX <- XXX[,-ncol(XXX)]
  #if(is.null(ALPHA)) ALPHA <- 1
  #if(is.null(CCC)) CCC <- 1.1
  #if(is.null(TARGETVARS)) TARGETVARS <- rep(1,ncol(XXX))
  #if(is.null(GGG)) GGG <- 0.01
  #if(is.null(QQQ)) QQQ <- .bdiag(as.list(rep(1,length(YYY))))
  ### debugging parameters
  ALPHA <- 1
  CCC <- 1.1
  TARGETVARS <- rep(1,ncol(XXX))
  GGG <- 0.01
  QQQ <- .bdiag(as.list(rep(1,length(YYY))))
  MINDIFF <- 10e-4
  MAXSIM <- 8
  ROWS <- QQQ@i+1
  COLS <- QQQ@j+1
  SIM <- 1
  DDD <- 1
  PENVECold <- rep(0,ncol(XXX))
  while(SIM<50)
  {
    LAMBDA <- 2*CCC*sqrt(nrow(XXX))*qnorm(1-GGG/(2*ncol(XXX)*log(nrow(XXX))))
    if(SIM==1) EEE <- YYY - mean(YYY)
    EMAT <- sparseMatrix(i=ROWS,j=COLS,x=EEE[ROWS]*EEE[COLS])
    PENVEC <- rep(1,ncol(XXX))
    prod1 <- Matrix::crossprod(EMAT,XXX)
    prod2 <- Matrix::crossprod(XXX,prod1)
    PENVEC <- Matrix::diag(prod2)
    PENVEC <- (PENVEC / nrow(XXX))**0.5
    #PENVEC <- diag( t(XXX)%*%EMAT%*%XXX / nrow(XXX) )**0.5
    #PENVEC <- Matrix::diag( Matrix::t(XXX) Matrix::%*% EMAT Matrix::%*%XXX / nrow(XXX) )**0.5
    #PENVEC <- rep(1,ncol(XXX))
    if(min(PENVEC)==0)
    {
    BAD <- colnames(XXX)[PENVEC==0]
    REPORT <- paste(paste0(BAD," has zero penalty.  consider dropping this variable."))
    REPORT <- paste0(REPORT,"\n")
    stop(REPORT)
    }
   ### end drop
    UPSILON_MAT <- .bdiag(as.list(PENVEC**-1))
    UPSILON_MAT_INV <- .bdiag(as.list(PENVEC))
   XXX_STAR <- XXX%*%UPSILON_MAT
    colnames(XXX_STAR) <- colnames(XXX)
   fit <- glmnet(XXX_STAR,YYY,
                 lambda=LAMBDA/(2*NROW(XXX)),
                 penalty.factor= TARGETVARS,
                 standardize=F,
                 intercept=T,
                 alpha=ALPHA,
                  family="gaussian")
    bLASSO <- coef(fit)
	ACTIVEREGRESSORS <- rownames(bLASSO)[which(as.numeric(bLASSO)!=0)]
	ACTIVEREGRESSORS <- intersect(ACTIVEREGRESSORS , colnames(XXX))
    ACTIVECOLUMNS <- which(colnames(XXX)%in%ACTIVEREGRESSORS)
    ACTIVECOLUMNS <- unique(c(ACTIVECOLUMNS,ncol(XCSR)))
    slmFit <- slm.fit(XCSR[,ACTIVECOLUMNS],YYY,tmpmax=100*nrow(XXX))
    YYYHAT <- slmFit$fitted
    EEE <- YYY-YYYHAT
    KEEPERS <- rownames(bLASSO)[which(as.numeric(bLASSO)!=0)]
    DDD <- sum(abs(PENVEC-PENVECold))
    #print(SIM)
    #print(DDD)
    if(DDD<MINDIFF) break
    if(MAXSIM<=SIM) break
    PENVECold <- PENVEC
    SIM <- SIM+1
  }
  slmCoef <- slmFit$coef
  bhat <- rep(0,length(bLASSO))
  namesbhat <- rownames(bLASSO)
  ids <- match(ACTIVEREGRESSORS , namesbhat)
  bhat[1] <- slmCoef[length(slmCoef)]
  bhat[ids] <- slmCoef[-length(slmCoef)]
  bhatdf <- data.frame( token=as.character(namesbhat) , estimate=round(bhat,3) )
  return( list(coef=bhatdf , residuals=slmFit$residuals) )
}
