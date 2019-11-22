tokenMatrixMaker <- function(TEXT , GRAM=NULL , KTOKEN=NULL)
{
require(tm)
require(Matrix)
require(stringr)
if(is.null(GRAM)) GRAM <- 1
if(is.null(KTOKEN)) KTOKEN <- 500
if(3<GRAM)
{
GRAM <- 3
print("Currently, tokenMatrixMaker only supports unigrams, bigrams, and trigrams.  Setting GRAM equal to 3.")
}
maxTokens <- KTOKEN
TEXT <- iconv(enc2utf8(TEXT),sub="byte")
TEXT <- gsub("\\s+"," ",TEXT)
Encoding(TEXT) <- "latin1"
TEXT <- iconv(TEXT, "latin1", "UTF-8",sub='')
TEXT <- trimws(TEXT)
### n-gram work
if(1<GRAM) TEXT <- str_split(TEXT," ")
### bigram
if(GRAM==2) TEXT <- sapply(TEXT , function(x) paste(paste(x,c(x[-1],"endOfText"),sep="XXX"),collapse=" "))
### trigram
if(GRAM==3) TEXT <- sapply(TEXT , function(x) paste(paste(x,c(x[-1],"endOfText"),c(x[-c(1,2)],"endOfText","endOfText"),sep="XXX"),collapse=" "))
### tdm
Corpus = Corpus(VectorSource(TEXT))
B1 <- 1
B2 <- Inf
C1 <- 2
C2 <- Inf
CONTROL <- list(stopwords=T, bounds=list(global = c(B1 , B2) , wordLengths=c(C1,C2)))
tdm <- DocumentTermMatrix(Corpus,
control =  CONTROL)
t2 <- Sys.time()
colnames(tdm) <- gsub("xxx","-",colnames(tdm))
ok <- which(!grepl("endoftext$",colnames(tdm)))
tdm <- tdm[,ok]
ok <- which(!grepl("endofsentence$",colnames(tdm)))
tdm <- tdm[,ok]
tdm <- tdm[,colnames(tdm)!="stop"]
colSumz <- table(tdm$j)
oSum <- order(-colSumz)
TOKENS <- min(ncol(tdm), maxTokens)
tdm <- tdm[, oSum[1:TOKENS]]
head(colnames(tdm))
TOKENMAT <- sparseMatrix(i=tdm$i , j=tdm$j , dims=c(nrow(tdm),ncol(tdm)))
colnames(TOKENMAT) <- colnames(tdm)
return(TOKENMAT)
}
