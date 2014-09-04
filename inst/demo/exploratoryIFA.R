library(OpenMx)
library(rpf)

# --------------------------------------------------------------- generate data

set.seed(1)  # ensure we can reproduce the results

# suppose our items are related to language development
factorNames <- c('spelling', 'phonics')

# generate random data for a 2 factor latent structure
numItems <- 20
spec <- list()  # a map from columns to item models

# rpf.grm creates a graded response model
spec[1:numItems] <- rpf.grm(factors=2)

# randomly generate the true item parameters
trueIMat <- mxSimplify2Array(lapply(spec, rpf.rparam))
rownames(trueIMat)[1:2] <- factorNames

# half the items mostly assess spelling and the other half of the items assess phonics
trueDiscrimination0 <- apply(trueIMat[1:2,], 2, norm, "2")
trueIMat['spelling',1:(numItems/2)] <- .1 * trueIMat['spelling',1:(numItems/2)]
trueIMat['phonics',(1+numItems/2):numItems] <- .1 * trueIMat['phonics',(1+numItems/2):numItems]

# we need to rescale the intercept to keep it in the original place
trueIMat['b',] <- apply(trueIMat[1:2,], 2, norm, "2") * trueIMat['b',] / trueDiscrimination0

# set the true latent structure
trueCovariance <- .2   # change this and see what happens
trueLatentCov <- matrix(c(1, trueCovariance, trueCovariance, 1), 2, 2)

# simulate some data
data <- rpf.sample(2000, spec, trueIMat, cov=trueLatentCov)
itemNames <- colnames(data)
data <- compressDataFrame(data)

# --------------------------------------------------------------- 1 factor

computePlan <- mxComputeSequence(list(
  mxComputeEM('expectation', 'scores', mxComputeNewtonRaphson()),  # optimize
  mxComputeOnce('fitfunction', 'information', "meat"),             # approx info matrix
  mxComputeHessianQuality(),
  mxComputeStandardError()))

spec[1:numItems] <- rpf.grm()
imat <- mxMatrix(name="item", nrow=2, ncol=numItems, values=c(1,0), free=TRUE,
                 dimnames=list(c("lang","b"), itemNames))

m1 <- mxModel(model="model1", imat,
              mxData(observed=data, type="raw", numObs=sum(data$freq)),
              mxExpectationBA81(ItemSpec=spec, weightColumn = "freq"),
              mxFitFunctionML(),
              computePlan)              
m1 <- mxRun(m1)

# --------------------------------------------------------------- 2 factors

spec[1:numItems] <- rpf.grm(factors = 2)
imat <- mxMatrix(name="item", nrow=3, ncol=numItems, values=c(1,1,0), free=TRUE,
                 dimnames=list(c(factorNames,"b"), itemNames))

# for identification, we need to fix 1 slope to 0
imat$values['spelling','i1'] <- 0
imat$free['spelling','i1'] <- FALSE

m2 <- mxModel(model="model2", imat,
              mxData(observed=data, type="raw", numObs=sum(data$freq)),
              mxExpectationBA81(ItemSpec=spec, weightColumn = "freq",
                                qpoints=31, qwidth=5),  # speed things up with fewer quadrature pts
              mxFitFunctionML(),
              computePlan)              
m2 <- mxRun(m2)

# --------------------------------------------------------------- compare fit

mxCompare(m2,m1)  # likelihood ratio test

# i12 has huge SEs so we can expect to see some bad stuff in fit diagnostics

# prepare IFAgroup objects for further diagnostics
m1G <- as.IFAgroup(m1)
m2G <- as.IFAgroup(m2)

# i12 indeed shows up as unusual
SitemFit(m1G)
SitemFit(m2G)

# We don't trust these tests, but here's how to do it
multinomialFit(m1G)
multinomialFit(m2G)

# fit is so bad, these are unhelpful
ChenThissen1997(m1G)
ChenThissen1997(m2G)
