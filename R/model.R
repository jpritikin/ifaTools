#' Replicate a model for each group of data
#'
#' The reference group is fixed to a zero mean and identity covariance matrix.
#' 
#' @param tmpl an OpenMx model
#' @param fullData the complete data including the column indicating group membership
#' @param mMat an MxMatrix for latent means
#' @param covMat an MxMatrix for latent covariance
#' @param ...  Not used.  Forces remaining arguments to be specified by name.
#' @param splitCol the name of the column used to indicate group membership
#' @param refGroup the name of the reference group
#' @param split whether to split the data (defaults to TRUE)
#' 
#' @export
replicateModelBy <- function(tmpl, fullData, mMat, covMat, ...,
			     splitCol="population", refGroup="general", split=TRUE) {
    garbageArguments <- list(...)
    if (length(garbageArguments) > 0) {
        stop("Values for the '...' argument are invalid; use named arguments")
    }

  dataCol <- setdiff(colnames(fullData), splitCol)
  itemMatName <- tmpl$expectation$item
  ivalues <- tmpl[[itemMatName]]$values
  newLabels <- tmpl[[itemMatName]]$labels
  
  for (r in 1:nrow(newLabels)) {
    for (c in 1:ncol(newLabels)) {
      if (!is.na(newLabels[r,c]) || is.na(ivalues[r,c])) next
      newLabels[r,c] <- paste(itemMatName,"_r", r, "c", c, sep = "")
    }
  }
  
  tmpl[[itemMatName]]$labels <- newLabels
  
  refData <- subset(fullData, !split | population==refGroup, dataCol)
  refModel <- mxModel(model=tmpl, name=refGroup,
                      mxData(observed=refData, type="raw"))

  extraGroups <- setdiff(unique(fullData[[splitCol]]), refGroup)
  container <- mxModel(model="container", refModel)
  if (!split) {
    extraGroups <- NULL
  } else {
    container <- mxModel(container,
                         mxModel(model="latentFit",
                                 mxFitFunctionMultigroup(paste0(extraGroups, "Latent"))))
  }

  for (grp in extraGroups) {
    gmodel <- mxModel(model=tmpl, name=grp, mMat, covMat,
                      mxData(observed=subset(fullData, population==grp, dataCol), type="raw"))
    lmodel <- mxModel(model=paste0(grp,"Latent"),
                      mxDataDynamic(type='cov', expectation=paste(grp,'expectation',sep='.')),
                      mxExpectationNormal(covariance=paste(grp,'cov',sep='.'),
                                          means=paste(grp,'mean',sep='.')),
                      mxFitFunctionML())
    container <- mxModel(model=container, gmodel, lmodel)
  }
  
  enames <- c(refGroup, extraGroups)
  mstep <- list(mxComputeNewtonRaphson(freeSet=paste(enames,'item',sep='.')))
  if (split) {
    lset <- apply(expand.grid(extraGroups, c("mean","cov")), 1, paste, collapse='.')
    mstep <- c(mstep,
               mxComputeGradientDescent(fitfunction='latentFit.fitfunction',
                                        freeSet=lset))
  }
  container <- mxModel(container,
                       mxFitFunctionMultigroup(enames),
                       mxComputeEM(paste(enames, 'expectation', sep="."), 'scores',
                                   mxComputeSequence(mstep)))
  container
}
