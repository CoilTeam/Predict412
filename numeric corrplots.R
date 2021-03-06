rm(list=ls())
library(kernlab)
library(corrplot)
library(Hmisc)


setwd("D:/R Working Directory/Predict412/teamproject")

# load data and check data integrity
data(ticdata)
nrow(ticdata)
tic <- ticdata[1:5822,] # Select training data only
head(tic)
class(tic)
rows <- nrow(tic)
compcases  <- sum(complete.cases(tic))
# if this logical test returns false, data is missing.
rows==compcases
# there is no missing data
dim(tic)
str(tic)
tic <- within(tic, CARAVAN <- as.numeric(CARAVAN))
numeric <- as.data.frame(which(sapply(tic, is.numeric)==TRUE))[,1]
ticnumeric <- tic[, numeric]
dim(ticnumeric)
data <- ticnumeric
rows  <- nrow(data)
#str(data)

# set up corr test function
cor.mtest <- function(mat, conf.level = 0.90){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

# set up correlations matrix with conf intervals
res1 <- cor.mtest(data,0.90)
res1

corrmat <- cor(data, method="pearson", use="complete.obs")
corrmat

# the lower half shows the correlation coefficient if it is significant at the alpha set below (here 0.1). Need to figure out how to reduce the size of these numbers or increase the size of each box to make them readable
corrplot(corrmat, type="upper", diag=TRUE,  order = "hclust",p.mat = res1[[1]], insig = "blank", sig.level = 0.10,tl.pos="lt",tl.cex=0.8, cl.cex=0.8, cl.ratio=0.2, cl.pos="r")
corrplot(corrmat, add=TRUE,type="lower", diag=FALSE,tl.pos="n", p.mat = res1[[1]], insig = "blank", method="number", order = "hclust", sig.level = 0.10, cl.pos="n")

# the lower half shows the correlation coefficient if it is significant at the alpha set below (here 0.1). Need to figure out how to reduce the size of these numbers or increase the size of each box to make them readable
corrplot(corrmat, type="upper", diag=TRUE,  order = "hclust",p.mat = res1[[1]], insig = "blank", sig.level = 0.10, tl.pos="lt", tl.cex=0.7, cl.cex=0.7, cl.ratio=0.2, cex.axis = 0.3, cl.pos="r", tl.offset=1)
corrplot(corrmat, add=TRUE,type="lower", diag=FALSE, tl.pos="n", p.mat = res1[[1]], insig = "blank", method="number", order = "hclust", sig.level = 0.10, cl.pos="n")


# alternate view - use ellipses until I can fix the lower corr coeff text size in the previous plot
corrplot(corrmat, method="ellipse", order = "hclust", tl.cex=0.7)
