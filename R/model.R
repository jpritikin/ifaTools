#' Adds exploratory factors to a single factor model
#'
#' @param model a single factor (possibly multigroup) model
#' @param toAdd the number of factors to add
#' @param ...  Not used.  Forces remaining arguments to be specified by name.
#' 
#' @export
addExploratoryFactors <- function(model, toAdd, ...) {
    garbageArguments <- list(...)
    if (length(garbageArguments) > 0) {
        stop("Values for the '...' argument are invalid; use named arguments")
    }

  ex <- model$expectation
  if (is(ex, "MxExpectationBA81")) {
    if (any(sapply(ex$ItemSpec, function(s) s$factors) != 1)) {
      stop(paste("Model", model$name, "has items that load",
                 "on more or less than 1 factor"))
    }
    ex$ItemSpec <- lapply(ex$ItemSpec, rpf.modify, factors=1+toAdd)
    item <- model[[ex$item]]
    if (is.null(item)) {
      stop(paste("Cannot find item matrix in model", model$name))
    }
    if (any(!is.na(item$lbound)) || any(!is.na(item$ubound))) {
      stop(paste("EFA for item parameters with bounds are not implemented"))
    }
    nitem <- mxMatrix(nrow=nrow(item)+toAdd, ncol=ncol(item),
                      name=item$name, dimnames=list(NULL, colnames(item)))
    nitem$values[1,] <- item$values[1,]
    nitem$values[(2+toAdd):nrow(nitem),] <- item$values[2:nrow(item),]
    nitem$labels[1,] <- item$labels[1,]
    nitem$labels[(2+toAdd):nrow(nitem),] <- item$labels[2:nrow(item),]
    nitem$free[1,] <- item$free[1,]
    nitem$free[(2+toAdd):nrow(nitem),] <- item$free[2:nrow(item),]
    rownames(nitem) <- c(rownames(item)[1],
                         paste0("explore", 1:toAdd),
                         rownames(item)[2:nrow(item)])
    
    for (fx in 1:toAdd) {
      lab <- paste0("E",fx)
      mask <- item$free[1,]
      mask[which(mask)[1:fx]] <- FALSE
      nitem$free[1+fx,] <- mask
      nitem$values[1+fx, mask] <- nitem$values[1, mask]
      mask <- mask & !is.na(nitem$labels[1,])
      nitem$labels[1+fx, mask] <- paste0(lab, nitem$labels[1, mask])
    }
    
    ex$qpoints <- min(ex$qpoints,
                      switch(as.character(1+toAdd),
                             '2'=31, '3'=11, '4'=7, 5))
    ex$qwidth <- min(ex$qwidth,
                      switch(as.character(1+toAdd),
                             '2'=5, '3'=4, '4'=3, 2))

    model <- mxModel(model, ex, nitem)
    
    mMat <- model[[ex$mean]]
    if (!is.null(mMat)) {
      nmMat <- mxMatrix(nrow=1, ncol=1+toAdd, values=0, free=TRUE,
                        name=mMat$name)
      colnames(nmMat) <- c(ifelse(length(rownames(mMat)),
                                  rownames(mMat), colnames(mMat)),
                           paste0("explore", 1:toAdd))
      # preserve labels? TODO
      model <- mxModel(model, nmMat)
    }

    cMat <- model[[ex$cov]]
    if (!is.null(cMat)) {
      ncMat <- mxMatrix(type="Symm", nrow=1+toAdd, ncol=1+toAdd,
                        values=diag(1+toAdd), free=TRUE,
                        dimnames=list(colnames(nmMat), colnames(nmMat)),
                        name=cMat$name)
      for (rx in 1:toAdd) {
        for (cx in (rx+1):(1+toAdd)) {
          lab <- paste(model$name, cMat$name,
                       paste0("r", rx, "c", cx), sep="_")
          ncMat$labels[rx,cx] <-
            ncMat$labels[cx,rx] <- lab
        }
      }
      model <- mxModel(model, ncMat)
    }
  }
 
  subs <- lapply(names(model$submodels),
                 function(sname) {
                   addExploratoryFactors(model[[sname]], toAdd)
                 })
  mxModel(model, subs)
}

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
#' @param compressData whether to apply compressDataFrame (defaults to TRUE)
#' 
#' @export
replicateModelBy <- function(tmpl, fullData, mMat, covMat, ...,
			     splitCol="population", refGroup="general", split=TRUE,
			     compressData=TRUE) {
    garbageArguments <- list(...)
    if (length(garbageArguments) > 0) {
        stop("Values for the '...' argument are invalid; use named arguments")
    }

  dataCol <- setdiff(colnames(fullData), splitCol)
  itemMatName <- tmpl$expectation$item
    if (compressData) {
	    tmpl$expectation$weightColumn <- 'freq'
    }
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
    if (compressData) {
	    refData <- compressDataFrame(refData)
    }
  refModel <- mxModel(model=tmpl, name=refGroup,
                      mxData(observed=refData, type="raw", sort=!compressData))
    if (compressData) {
	    refModel$data$numObs <- sum(refData[['freq']])
    }
    
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
	  gdata <- subset(fullData, population==grp, dataCol)
	  if (compressData) {
		  gdata <- compressDataFrame(gdata)
	  }
	  gmodel <- mxModel(model=tmpl, name=grp, mMat, covMat,
			    mxData(observed=gdata, type="raw", sort=!compressData))
	  if (compressData) {
		  gmodel$data$numObs <- sum(gdata[['freq']])
	  }
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
