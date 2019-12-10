library(OpenMx)
library(rpf)
library(ifaTools)
library(testthat)

set.seed(1)

context("addExploratoryFactor")

#print(getwd())

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
data <- compressDataFrame(data, .asNumeric=TRUE)
itemModel <- mxModel(model='itemModel', imat,
           mxData(observed=data, type='raw', weight='freq'),
           mxExpectationBA81(ItemSpec=spec),
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
  m1 <- mxRun(m1, silent=TRUE)  # not 100% reliable
  #cat(deparse(round(coef(m1),2)))
  pt1 <- c(p1 = 1.81, p2 = -0.51, V1_g = -1.14, p3 = 1.24, p4 = 2.58,  V2_g = -1.28, p5 = 1.55,
           p6 = -1.03, V3_g = -1.16, p7 = 1.36,  p8 = 0.42, V4_g = -1.1, p9 = 1.41, p10 = -0.46,
           V5_g = -1.03,  p11 = 1.5, p12 = 1.84, V6_g = -1.43, slope = 1.12, p13 = 3.5,  p14 = 1.57,
           p15 = 2.7, p16 = 2.27, p17 = -0.28, p18 = 0.15)
  m1 <- omxSetParameters(m1, names(pt1), values=pt1)
  m1p <- mxRun(mxModel(m1, mxComputeOnce('fitfunction', 'fit')))
  expect_equal(m1p$output$fit, 33454.39, .1)
  
  m2 <- addExploratoryFactors(container, 1)
  m2 <- mxRun(m2, silent=TRUE)
  pt2 <- c(p1 = 2.02, p2 = -0.39, V1_g = -1.3, p3 = 0.8, E1p1 = 1.24,  p4 = 2.75, V2_g = -1.18,
           p5 = 1.4, E1p2 = 0.4, p6 = -0.91, V3_g = -1.25,  p7 = 1.22, E1p3 = 0.47, p8 = 0.51,
           V4_g = -1.28, p9 = 1.27, E1p4 = 0.61,  p10 = -0.46, V5_g = -1.04, p11 = 1.33, E1p5 = 0.67,
           p12 = 1.84,  V6_g = -1.41, slope = 0.94, E1p6 = 1.87, p13 = 4.58, E1p7 = 0.43,  p14 = 1.54,
           E1p8 = 1.73, p15 = 3.46, E1p9 = 1.18, p16 = 2.55,  E1p10 = 0.18, p17 = -0.27, E1p11 = 0.36,
           p18 = 0.15)
  m2 <- omxSetParameters(m2, names(pt2), values=pt2)
  m2p <- mxRun(mxModel(m2, mxComputeOnce('fitfunction', 'fit')))
  expect_equal(m2p$output$fit, 33338.99, .1)
})
