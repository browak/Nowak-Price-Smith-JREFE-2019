lassoPostLasso <- function(XXX , TOKENMAT , YYY)
{
require(glmnet)
FOLDS <- sample(1:5,length(YYY),replace=T)
WWW <- cBind(XXX, TOKENMAT)

### baseline
baseline <- postLasso( XXX , YYY )
rmse_baseline <- sd(baseline$residuals)
mae_baseline <- mean(abs(baseline$residuals))

### cv
cvFitTokensGLMNET <- cv.glmnet( WWW , YYY , foldid=FOLDS , parallel=F)

### min
B <- coef(cvFitTokensGLMNET , s= cvFitTokensGLMNET$lambda.min)
S <- intersect( rownames(B)[which(as.numeric(B)!=0)],colnames(TOKENMAT))
Q <- length(S)
X <- cBind( XXX , TOKENMAT[,S] )
fitMin <- postLasso( X , YYY )
rmse_min <- sd(fitMin$residuals)
mae_min <- mean(abs(fitMin$residuals))
S_min <- S
Q_min <- length(S)
dictionary_min <- data.frame(token=rownames(B),estimate=as.numeric(B))
dictionary_min <- dictionary_min[dictionary_min$token%in%S_min,]
dictionary_min <- dictionary_min[order(dictionary_min$token),]
dictionary_min$estimate <- round(dictionary_min$estimate,3)
rownames(dictionary_min) <- NULL

### het
hetFit <- clusterLasso(WWW,YYY)
B <- hetFit$coef
S_het <- intersect( B$token[which(B$estimate!=0)],colnames(TOKENMAT))
Q_het <- length(S_het)
dictionary_het <- B[B$token%in%S_het,]
dictionary_het <- dictionary_het[order(dictionary_het$token),]
dictionary_het$estimate <- round(dictionary_het$estimate,3)
rownames(dictionary_het) <- NULL
if(0<length(Q_het))
{
X <- cBind( XXX , TOKENMAT[,S_het] )
fitHet <- postLasso( X , YYY )
rmse_het <- sd(fitHet$residuals)
mae_het <- mean(abs(fitHet$residuals))
} else {
rmse_het <- 0
mae_het <- 0
}

dfFitted <- data.frame(original=y,hetFitted=fitHet$fitted,cvFitted=fitMin$fitted)
df <- data.frame(N=nrow(TOKENMAT),K=ncol(TOKENMAT),
Q_min,Q_het,
rmse_baseline,rmse_min,rmse_het,
mae_baseline,mae_min,mae_het)
return(list(predictionInformation=df ,
            hetTokens=S_het ,
            cvTokens=S_min ,
            hetPostLassoFit=fitHet ,
            cvPostLassoFit=fitMin ,
            hetDictionary=dictionary_het ,
            cvDictionary=dictionary_min ,
            fittedValues=dfFitted))
}
