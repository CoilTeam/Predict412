rm(list=ls())
data <- as.data.frame(read.csv("D:/R Working Directory/Git/Predict412/ticdata2000.csv"))

head(data)

layout(matrix(1:4,2,2))
for(n in names(data)){
  hist(data[,n], main = n, xlab = n)
}


# check for missing data
rows  <- nrow(data)
compcases  <- sum(complete.cases(data))
# if this next logical test returns FALSE, data is missing.
rows==compcases
# no issues, all rows are complete

# Attribute 86, "CARAVAN:Number of mobile home policies", is the target variable.
summary(data$caravan)
posresp  <- sum(data$caravan)
posprop  <- round((posresp/rows), digits =4)
posprop
# so positive response (purchased a mobile home policy) is low: ~6%


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



# set up expanded correlation matrix with conf intervals
library(corrplot)

# this is another correlation matrix, but I want the simplest cor() version here for use later
corrmat <- cor(data, method="pearson", use="complete.obs")
corrmat

res1 <- cor.mtest(data,0.90)


# the lower half shows the correlation coefficient if it is significant at the alpha set below (here 0.1). Need to figure out how to reduce the size of these numbers or increase the size of each box to make them readable
# syntax for mar is  c(bottom, left, top, right) 

corrplot(corrmat, type="upper", mar=c(3, 2, 2, 0), diag=TRUE,  order = "hclust",p.mat = res1[[1]], insig = "blank", sig.level = 0.10, tl.pos="lt", tl.cex=0.6, cl.cex=0.6, cl.ratio=0.2, cex.axis = 0.3, cl.pos="r", tl.offset=1)

# Need to figure out how to reduce the size of these numbers or increase the size of each box to make them readable.  For now, comment this version of the lower half out and swap in ellipses

# corrplot(corrmat, add=TRUE,type="lower", diag=FALSE, tl.pos="n", p.mat = res1[[1]], insig = "blank", method="number", order = "hclust", sig.level = 0.10, cl.pos="n", tl.cex=0.2)

corrplot(corrmat, add=TRUE,type="lower", diag=FALSE, tl.pos="n", p.mat = res1[[1]], insig = "blank", method="ellipse", order = "hclust", sig.level = 0.10, cl.pos="n")


# alternate view - use ellipses until I can fix the lower corr coeff text size in the previous plot
corrplot(corrmat, method="ellipse", mar=c(3, 2, 2, 0), order = "hclust", p.mat = res1[[1]], sig.level = 0.10, insig = "blank", tl.cex=0.6, tl.offset=1)


