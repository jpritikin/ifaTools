library(OpenMx)
library(rpf)
library(ifaTools)
library(GPArotation)

# --------------------------------------------------------------- generate data

set.seed(1)  # ensure we can reproduce the results

# suppose our items are related to language development
factorNames <- c('spelling', 'phonics')

# generate random data for a 2 factor latent structure
numItems <- 20
spec <- list()  # a map from columns to item models

# rpf.grm creates a graded response model
spec[1:numItems] <- rpf.grm(factors=2, outcomes=5)

trueIMat <- matrix(1.5, nrow=6, ncol=numItems)
trueIMat[3,] <- seq(-2,2, length.out = numItems/2) + .5
trueIMat[4,] <- seq(-2,2, length.out = numItems/2) + .1
trueIMat[5,] <- seq(-2,2, length.out = numItems/2) - .1
trueIMat[6,] <- seq(-2,2, length.out = numItems/2) - .5
rownames(trueIMat) <- c(factorNames, paste0("b",1:4))
colnames(trueIMat) <- paste0('i', 1:numItems)
names(spec) <- colnames(trueIMat)

# half the items mostly assess spelling and the other half of the items assess phonics
trueDiscrimination0 <- apply(trueIMat[1:2,], 2, norm, "2")
trueIMat['spelling',1:(numItems/2)] <- 0
trueIMat['phonics',(1+numItems/2):numItems] <- 0

# we need to rescale the intercept to keep it in the original place
trueIMat[3:6,] <- apply(trueIMat[1:2,], 2, norm, "2") * trueIMat[3:6,] / trueDiscrimination0

# simulate some data
if (1) {  # hard
  basis1 <- runif(2)
  basis1 <- basis1 / norm(basis1, "2")
  basis2 <- c()
  for (retry in 1:10) {
    basis2 <- runif(2)
    basis2 <- basis2 / norm(basis2, "2")
    if (basis1 %*% basis2 < .8) break
  }
  variance <- rlnorm(2)
} else {  # easy
  basis1 <- c(1,0)
  basis2 <- c(0,1)
  variance <- c(1,1) #rlnorm(2)
}

theta <- rbind(t(as.matrix(basis1) %*% rnorm(1000, sd=sqrt(variance[1]))),
               t(as.matrix(basis2) %*% rnorm(1000, sd=sqrt(variance[2]))))
#theta <- t(as.matrix(basis1) %*% rnorm(1000, sd=sqrt(variance[1])))
data <- rpf.sample(t(theta), spec, trueIMat)
itemNames <- colnames(data)
data <- compressDataFrame(data)

if (0) {
  # Note severe signal degredation even using the true item parameters!
  plot(theta)
  tgrp <- list(spec=spec, param=trueIMat, data=data, weightColumn="freq")
  require(ggplot2)
  sc <- as.data.frame(EAPscores(tgrp)[,1:2])
  ggplot(sc, aes(x=spelling, y=phonics)) + geom_point(size=3, alpha=.15)
}

# --------------------------------------------------------------- 1 factor

computePlan <- mxComputeSequence(list(
  mxComputeEM('expectation', 'scores', mxComputeNewtonRaphson()),  # optimize
  mxComputeOnce('fitfunction', 'information', "meat"),             # approx info matrix
  mxComputeHessianQuality(),
  mxComputeStandardError()))

spec[1:numItems] <- lapply(spec, rpf.modify, factors=1)

imat <- mxMatrix(name="item", free=TRUE,
                 values=mxSimplify2Array(lapply(spec, rpf.rparam)))

m1 <- mxModel(model="model1", imat,
              mxData(observed=data, type="raw", numObs=sum(data$freq)),
              mxExpectationBA81(ItemSpec=spec, weightColumn = "freq"),
              mxFitFunctionML(),
              computePlan)              
m1 <- mxRun(m1)
m1G <- as.IFAgroup(m1)

# --------------------------------------------------------------- 2 factors

m2 <- addExploratoryFactors(m1, 1)
m2 <- mxRun(m2)
m2G <- as.IFAgroup(m2)

# --------------------------------------------------------------- 3 factors

m3 <- addExploratoryFactors(m1, 2)
m3 <- mxRun(m3)
m3G <- as.IFAgroup(m3)

# --------------------------------------------------------------- compare fit

m0 <- mxRefModels(m1, run=TRUE)

mxCompare(m3,c(m2,m1,m0$Independence))  # likelihood ratio test

if (0) {
  
  m2G$param[1:2,] <- m2$item$values[1:2,]
  plot(EAPscores(m2G)[,1:2])
  m2G$param[1:2,] <- fromFactorLoading(oblimin(toFactorLoading(m2$item$values[1:2,]))$loadings)
  plot(EAPscores(m2G)[,1:2])
  m2G$param[1:2,] <- fromFactorLoading(geominT(toFactorLoading(m2$item$values[1:2,]))$loadings)
  plot(EAPscores(m2G)[,1:2])
  m2G$param[1:2,] <- fromFactorLoading(bentlerQ(toFactorLoading(m2$item$values[1:2,]))$loadings)
  plot(EAPscores(m2G)[,1:2])
  m2G$param[1:2,] <- fromFactorLoading(bentlerT(toFactorLoading(m2$item$values[1:2,]))$loadings)
  plot(EAPscores(m2G)[,1:2])
  m2G$param[1:2,] <- fromFactorLoading(simplimax(toFactorLoading(m2$item$values[1:2,]))$loadings)
  plot(EAPscores(m2G)[,1:2])
  m2G$param[1:2,] <- fromFactorLoading(cfQ(toFactorLoading(m2$item$values[1:2,]))$loadings)
  plot(EAPscores(m2G)[,1:2])
  #             h2 <- rowSums(Flist$F^2)
}

if (0) {
  SitemFit(m1G)
  SitemFit(m2G)
  
  # We don't trust these tests, but here's how to do it
  multinomialFit(m1G)
  multinomialFit(m2G)
  
  ChenThissen1997(m1G)
  ChenThissen1997(m2G)
}

if (0) {
  require("mirt")
  mdata <- expandDataFrame(as.data.frame(lapply(data, unclass)), freqName = "freq")
  mod <- mirt(mdata, 2, TOL=c(.001, .01))
  summary(mod, rotate="none")
  
  clist <- coef(mod)
  clist$GroupPars <- NULL
  FL <- toFactorLoading(mxSimplify2Array(clist)[1:2,])
}
