lassoPostLassoJREFE <- function(WWW0 , TOKEN1 , YYY , WWW1)
{
FOLDS <- sample(1:10,length(YYY),replace=T)
WWW <- cBind(WWW0, TOKEN1)
if(is.null(WWW1)) WWW1 <- WWW0

### baseline
baseline <- postLasso( WWW1 , YYY )
rmse_baseline <- sd(baseline$residuals)
mae_baseline <- mean(abs(baseline$residuals))

### cv
cvFitTokensGLMNET <- cv.glmnet( WWW , YYY , foldid=FOLDS , parallel=T)

### min
B <- coef(cvFitTokensGLMNET , s= cvFitTokensGLMNET$lambda.min)
S <- intersect( rownames(B)[which(B!=0)],colnames(TOKEN1))
Q <- length(S)
X <- cBind( WWW1 , TOKEN1[,S] )
fit <- postLasso( X , YYY )
rmse_min <- sd(fit$residuals)
mae_min <- mean(abs(fit$residuals))
S_min <- S
Q_min <- length(S)

### 1se
B <- coef(cvFitTokensGLMNET , s= cvFitTokensGLMNET$lambda.1se)
S <- intersect( rownames(B)[which(B!=0)],colnames(TOKEN1))
Q <- length(S)
X <- cBind( WWW1 , TOKEN1[,S] )
fit <- postLasso( X , YYY )
rmse_1se <- sd(fit$residuals)
mae_1se <- mean(abs(fit$residuals))
S_1se <- S
Q_1se <- length(S)

### het
hetFit <- clusterLasso(WWW,YYY)
B <- hetFit$coef
S_het <- intersect( rownames(B)[which(B!=0)],colnames(TOKEN1))
Q_het <- length(S_het)
if(0<length(Q_het))
{
X <- cBind( WWW1 , TOKEN1[,S_het] )
fit <- postLasso( X , YYY )
rmse_het <- sd(fit$residuals)
mae_het <- mean(abs(fit$residuals))
} else {
rmse_het <- 0
mae_het <- 0
}

hetTokenList <- paste(S_het,collapse="XXX")

df <- data.frame(N=nrow(TOKEN1),K=TOKENS,GRAM,
Q_min,Q_1se,Q_het,
rmse_baseline,rmse_min,rmse_1se,rmse_het,
mae_baseline,mae_min,mae_1se,mae_het,hetTokenList)
return(df)
}