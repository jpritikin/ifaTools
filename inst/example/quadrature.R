library(ifaTools)
library(ggplot2)

if (0) {
  control <- expand.grid(
    # conditions
    qpoints = seq(9,17,1), qwidth = seq(3,5,.5), dataFactors = 2, modelFactors = 2, seed=1,
    # outcomes
    fit= NA, code = NA, grad=NA, posdef = NA, condnum=NA
  )
  model <- NULL
}

mkmodel <- function(seed, dataFactors, modelFactors) {
  set.seed(seed)
  
  # generate random data for a 2 factor latent structure
  numItems <- 12
  spec <- list()  # a map from columns to item models
  
  # rpf.grm creates a graded response model
  spec[1:numItems] <- rpf.grm(dataFactors, outcomes=5)
  
  trueIMat <- matrix(1.5, nrow=4+dataFactors, ncol=numItems)
  trueIMat[1+dataFactors,] <- seq(-2,2, length.out = numItems/2)
  for (rx in (1+dataFactors):(4+dataFactors)) {
    trueIMat[rx,] <- trueIMat[rx-1,] - .3
  }
  rownames(trueIMat) <- c(paste0('f',1:dataFactors), paste0("b",1:4))
  colnames(trueIMat) <- paste0('i', 1:numItems)
  names(spec) <- colnames(trueIMat)
  
  perFactor <- 500

  data <- rpf.sample(perFactor * dataFactors, spec, trueIMat)

  data <- compressDataFrame(data)
  
  spec[1:numItems] <- lapply(spec, rpf.modify, factors=1)

  computePlan <- mxComputeSequence(list(
    mxComputeEM('modelItem.expectation', 'scores', mxComputeNewtonRaphson(),
                verbose=0, information='oakes1999',
                infoArgs=list(fitfunction='fitfunction')),
    mxComputeHessianQuality(),
    mxComputeOnce('fitfunction', 'gradient'),
    mxComputeReportDeriv()))
  
  imat <- mxMatrix(name="item", free=TRUE,
                   values=mxSimplify2Array(lapply(spec, rpf.rparam)))
  imat$labels[,] <- paste0('p',1:prod(dim(imat)))
  
  template <- mxModel(model="model", imat,
                      mxMatrix(values=seed, nrow=1, ncol=1, name="seed"),
                      mxMatrix(values=trueIMat, name="trueItem"),
                      mxData(observed=data, type="raw", numObs = perFactor*dataFactors,
                             sort = FALSE),
                      mxExpectationBA81(ItemSpec=spec, weightColumn = "freq"),
                      mxFitFunctionML(),
                      computePlan)

  addExploratoryFactors(template, modelFactors-1)
}

model <- NULL
for (cx in head(which(is.na(control$fit)),1):nrow(control)) {
  cntl <- as.list(control[cx,])
  if (is.null(model) || model$modelItem$seed$values != cntl$seed ||
      model$modelItem$expectation$ItemSpec[[1]]$factors != cntl$modelFactors) {
    model <- mkmodel(cntl$seed, cntl$dataFactors, cntl$modelFactors)
  }
  
  model$modelItem$expectation$qpoints <- cntl$qpoints
  model$modelItem$expectation$qwidth <- cntl$qwidth
  
  fit <- mxRun(model, suppressWarnings = TRUE, silent = TRUE)
  control[cx,'fit'] <- fit$output$fit
  control[cx,'code'] <- fit$output$status$code
  control[cx,'posdef'] <- fit$output$infoDefinite
  control[cx,'condnum'] <- fit$output$conditionNumber
  control[cx,'grad'] <- norm(fit$output$gradient, "2")
  
  gcontrol <- subset(control, !is.na(fit) & posdef)
#  gcontrol$fit <- scale(control$fit - median(control$fit))
  if (nrow(gcontrol) > 3) {
    pl <- ggplot(gcontrol,
                 aes(x=qpoints, y=qwidth, fill=condnum)) + geom_tile()
    print(pl)
  }
}
