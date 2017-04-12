library(OpenMx)
library(rpf)
library(ifaTools)
library(testthat)

set.seed(1)

context("addExploratoryFactor")

print(getwd())

data <- read.csv(file='g341-19.csv',
                 header=FALSE,sep='',quote="",
                 stringsAsFactors=FALSE,check.names=FALSE)

colnames(data) <- mxMakeNames(colnames(data), unique=TRUE)

factors <- "ability"
numFactors <- length(factors)
spec <- list()
spec[1:6] <- list(rpf.drm(factors=numFactors))
spec[7:12] <- list(rpf.grm(factors=numFactors, outcomes=2))
names(spec) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", 
  "V11", "V12")

missingColumns <- which(is.na(match(names(spec), colnames(data))))
if (length(missingColumns)) {
  stop(paste('Columns missing in the data:', omxQuotes(names(spec)[missingColumns])))
}

data[names(spec)] <- mxFactor(data[names(spec)], levels = 0:1, labels=c("incorrect", "correct"))

#set.seed(1)   # uncomment to get the same starting values every time
startingValues <- mxSimplify2Array(lapply(spec, rpf.rparam))
rownames(startingValues) <- paste0('p', 1:nrow(startingValues))
rownames(startingValues)[1:numFactors] <- factors

imat <- mxMatrix(name='item', values=startingValues, free=!is.na(startingValues))
imat$free['p4',1:6] <- FALSE
imat$values['p4',1:6] <- Inf
imat$labels['ability',7:12] <- 'slope'
imat$labels['p3',1:1] <- 'V1_g'
imat$labels['p3',2:2] <- 'V2_g'
imat$labels['p3',3:3] <- 'V3_g'
imat$labels['p3',4:4] <- 'V4_g'
imat$labels['p3',5:5] <- 'V5_g'
imat$labels['p3',6:6] <- 'V6_g'
hasLabel <- !is.na(imat$labels)
for (lab1 in unique(imat$labels[hasLabel])) {
  imat$values[hasLabel & imat$labels==lab1] <- 
    sample(imat$values[hasLabel & imat$labels==lab1], 1)
}
data <- compressDataFrame(data)
itemModel <- mxModel(model='itemModel', imat,
           mxData(observed=data, type='raw', numObs=sum(data[['freq']]), sort=FALSE),
           mxExpectationBA81(ItemSpec=spec, weightColumn='freq'),
           mxFitFunctionML())

priorLabels <- c("V1_g", "V2_g", "V3_g", "V4_g", "V5_g", "V6_g")
priorMode <- rep(NA, length(priorLabels))
priorMode[1:6] <- logit(1/4)
priorModel <- univariatePrior('logit-norm', priorLabels, priorMode)
container <- mxModel(model='container', itemModel, priorModel,
  mxFitFunctionMultigroup(groups=c('itemModel.fitfunction', 'univariatePrior.fitfunction')))

emStep <- mxComputeEM('itemModel.expectation', 'scores',
  mxComputeNewtonRaphson(), verbose=0L,
  information='oakes1999', infoArgs=list(fitfunction='fitfunction'))
computePlan <- mxComputeSequence(list(EM=emStep,
         HQ=mxComputeHessianQuality(),
         SE=mxComputeStandardError()))
container <- mxModel(container, computePlan)

test_that("addExploratoryFactors", {
  m1 <- addExploratoryFactors(container, 0)
  m1 <- mxRun(m1, silent=TRUE)
  m2 <- addExploratoryFactors(container, 1)
  m2 <- mxRun(m2, silent=TRUE)

  expect_equal(m1$output$fit - m2$output$fit, 115.39, .1)
})
