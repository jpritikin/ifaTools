library(shiny)
library(OpenMx)
library(rpf)

# add demo data TODO
# mention citation
# drm == dichotomous help TODO

#options(shiny.reactlog=TRUE)

verbose <- TRUE
allItemsToken <- '*ALL*'

# Use stupid nouns here to encourage folks to change them
# to more sensible ability labels.
sillyFactorName <- c('teacup','puppy','oink','sun','moon')

itemToSpec <- function(item) {
#  cat("itemToSpec", item$model, item$outcomes, item$factors, fill=T)
  if (is.null(item$model)) browser()
  outcomes <- item$outcomes
  factors <- item$factors
  switch(item$model,
         'drm' = rpf.drm(factors = factors),
         'grm' = rpf.grm(outcomes, factors = factors),
         'nrm' = rpf.nrm(outcomes, factors = factors, T.a=item$Ta, T.c=item$Tc))
}

mergeDataAndModel <- function(col, data, factors, item) {
  outcomes <- length(levels( data[[col]] ))
  if (outcomes < 2) return(item)  # will complain later
  if (is.null(item) ||
        (outcomes > 2 && item$model == 'drm') ||
        (outcomes == 2 && item$model == 'nrm')) {
    #    if (verbose) cat("set default model", fill=T)
    spec <- rpf.grm(outcomes, factors = factors)
    np <- rpf.numParam(spec)
    return(list(name=col, model='grm', outcomes=outcomes, factors=factors,
                starting=rpf.rparam(spec), labels=rep(NA, np), free=rep(TRUE, np), prior=rep(NA, np)))
  }
  if (item$model == 'nrm') {
    if (is.null(item$Ta)) item$Ta <- "trend"
    if (is.null(item$Tc)) item$Tc <- "trend"
  } else {
    item$Ta <- NULL
    item$Tc <- NULL
  }
  item$name <- col
  item$outcomes <- outcomes
  item$factors <- factors
  spec <- itemToSpec(item)
  np <- rpf.numParam(spec)
  if (length(item$starting) != np) {
    newSV <- rpf.rparam(spec)
    preserveMap <- match(names(newSV), item$starting)
    newInd <- which(!is.na(preserveMap))
    preserveMap <- preserveMap[!is.na(preserveMap)]
    oldItem <- item
    item$starting <- newSV
    item$free <- rep(TRUE, np)
    item$labels <- rep(NA, np)
    item$prior <- rep(NA, np)
    item$starting[newInd] <- oldItem$starting[preserveMap]
    item$free[newInd] <- oldItem$free[preserveMap]
    item$labels[newInd] <- oldItem$labels[preserveMap]
    item$prior[newInd] <- oldItem$prior[preserveMap]
  }
  item
}

changeItemModel <- function(itemModel, input, factorData, fi, newModel) {
  oldModel <- isolate(itemModel[[fi]]$model)
  if (is.null(oldModel)) return()
  if (newModel != oldModel) {
    if (verbose) cat("change",fi,"from", oldModel,"to", newModel,fill=T)
    item <- isolate(itemModel[[fi]])
    item$model <- newModel
    data <- isolate(factorData$val)
    numFactors <- isolate(input$numFactors)
    itemModel[[fi]] <- mergeDataAndModel(fi, data, numFactors, item)
  }
}

getFactorNames <- function(input) {
  numFactors <- isolate(input$numFactors)
  sapply(1:numFactors, function (x) input[[ paste0("nameOfFactor", x) ]])
}

buildParameterTable <- function(input, factorData, itemModel, attr) {
  data <- isolate(factorData$val)
  fi <- input$focusedItem
  if (fi == allItemsToken) {
    dcols <- setdiff(colnames(data), input$freqColumnName)
  } else {
    dcols <- fi
  }
  massign <- lapply(dcols, function (col) {
    im <- itemModel[[col]]
    v <- im[[attr]]
    names(v) <- names(im$starting)
    v
  })
  names(massign) <- dcols
  tbl <- mxSimplify2Array(massign)
  fnames <- getFactorNames(input)
  rownames(tbl)[1:length(fnames)] <- fnames
  tbl
}

getFocusedItem <- function(input, itemModel) {
  fi <- input$focusedItem
  inames <- names(itemModel)
  if (length(inames) == 0) return()
  
  if (fi == allItemsToken) {
    im <- itemModel[[ inames[1] ]]
  } else {
    im <- itemModel[[fi]]
  }
  im
}

getFocusedParameterNames <- function(input, im) {
  pname <- names(im$starting)
  fnames <- getFactorNames(input)
  pname[1:length(fnames)] <- fnames
  pname
}

computeFreeSelected <- function(im, fx) {
  sel <- "Free"
  isFree <- im$free[fx]
  if (!isFree) {
    if (is.finite(im$starting[fx])) {
      sel <- im$starting[fx]
    } else {
      sel <- "inf"
    }
  }
  sel
}

maybeUpdateFree <- function(input, itemModel, im, pname) {
  fx <- match(pname, names(im$starting))
  if (is.na(fx)) return()
  spec <- itemToSpec(im)
  sel <- computeFreeSelected(im, fx)
  if (sel == input$focusedParameterFree) return()
  
  if (input$focusedParameterFree == "Free") {
    im$free[fx] <- TRUE
    im$starting[fx] <- rpf.rparam(spec)[fx]
  } else {
    im$free[fx] <- FALSE
    pi <- rpf.paramInfo(spec, fx)
    if (pi$type == 'bound' && input$focusedParameterFree == 'inf') {
      if (names(im$starting[fx]) == 'g') {
        im$starting[fx] <- qlogis(0)
      } else {
        im$starting[fx] <- qlogis(1)
      }
    } else {
      im$starting[fx] <- as.numeric(input$focusedParameterFree)
    }
  }
  itemModel[[ im$name ]] <- im
}

maybeUpdateLabel <- function(input, itemModel, im, pname) {
  newLabel <- isolate(input$focusedParameterLabel)
  fx <- match(pname, names(im$starting))
  if (is.na(fx)) return()
  sel <- im$labels[fx]
  if (is.na(sel)) sel <- "none"
  if (sel == newLabel) return()
  
  if (newLabel == "none") {
    im$labels[fx] <- NA
  } else {
    im$labels[fx] <- newLabel
  }
  itemModel[[ im$name ]] <- im
}

toScript <- function(input, rawData, factorData, itemModel) {
  if (is.null(rawData$name)) {
    loadData <- paste(c("# load some demonstration data",
                        as.character(rawData$loadDemo)), collapse="\n")
  } else {
    loadData <- c("# Adjust the path in the next statement to load your data\n",
                  paste0("data <- read.csv(file='",rawData$name,"'"))
    if (!input$dataHeader) loadData <- c(loadData, ",header=FALSE")
    if (input$dataSep != ",") {
      loadData <- c(loadData, paste0(",sep='", input$dataSep, "'"))
    }
    if (input$dataQuote != '"') {
      loadData <- c(loadData, paste0(',quote="', input$dataQuote, '"'))
    }
    loadData <- c(loadData, ",stringsAsFactors=FALSE,check.names=FALSE)\n",
                  "colnames(data) <- mxMakeNames(colnames(data), unique=TRUE)")
  }

  data <- factorData$val
  ncols <- ncol(data)
  dcols <- setdiff(colnames(data), input$freqColumnName)
  mkSpec <- do.call(paste, c(lapply(dcols, function (col) {
    im <- itemModel[[col]]
    str <- c("  '", col, "'=rpf.", im$model, "(factors=numFactors")
    if (im$model != "drm") {
      str <- c(str, ",outcomes=", im$outcomes)
    }
    if (im$model == "nrm") {
      str <- c(str, ",T.a='", im$Ta, "', T.c='", im$Tc, "'")
    }
    paste0(c(str, ")"), collapse="")
  }), sep=",\n"))
  
  fnames <- getFactorNames(input)
  
  starting <- mxSimplify2Array(lapply(dcols, function(col) { itemModel[[col]]$starting }))
  free <- mxSimplify2Array(lapply(dcols, function(col) { itemModel[[col]]$free }))
  free[is.na(free)] <- TRUE  # can ignore these
  labels <- mxSimplify2Array(lapply(dcols, function(col) { itemModel[[col]]$labels }))
  labels[!free] <- NA

  rnames <- paste0('p', 1:nrow(starting))
  rnames[1:length(fnames)] <- fnames
  
  itemInit <- list()
  for (rx in 1:nrow(free)) {
    rname <- paste0("'", rnames[rx], "'")
    fcode <- rle(!free[rx,])
    mask <- fcode$values
    starts <- cumsum(c(1, fcode$lengths))[c(mask,FALSE)]
    lens <- fcode$lengths[mask]
    if (length(starts) == 0) {
      next
    }
    if (length(starts) == 1 && lens == ncol(free)) {
      itemInit <- c(itemInit, paste0("imat$free[",rname,",] <- FALSE"))
    } else {
      itemInit <- c(itemInit, mapply(function(start, len) {
        paste0("imat$free[",rname,",",start,":",(start+len),"] <- FALSE")
      }, starts, lens - 1L, SIMPLIFY=FALSE))
    }
    srow <- starting[rx,]
    srow[free[rx,]] <- NA
    code <- rle(srow)
    mask <- !is.na(code$value)
    itemInit <- c(itemInit, mapply(function(start, len) {
      paste0("imat$values[",rname,",",start,":",(start+len),"] <- ", srow[start])
    }, cumsum(c(1, code$lengths))[c(mask,FALSE)], code$lengths[mask] - 1L, SIMPLIFY=FALSE))
  }

  for (rx in 1:nrow(free)) {
    rname <- paste0("'", rnames[rx], "'")
    code <- rle(labels[rx,])
    mask <- !is.na(code$values)
    val <- code$values[mask]
    starts <- cumsum(c(1, code$lengths))[c(mask,FALSE)]
    lens <- code$lengths[mask]
    if (length(starts) == 0) next
    if (length(starts) == 1 && lens == ncol(free)) {
      itemInit <- c(itemInit, paste0("imat$labels[",rname,",] <- '",val[1],"'"))
    } else {
      itemInit <- c(itemInit, mapply(function(start, len, v1) {
        paste0("imat$labels[",rname,",",start,":",(start+len),"] <- '",v1,"'")
      }, starts, lens - 1L, val, SIMPLIFY=FALSE))
    }
  }
  
  itemInit <- paste(itemInit, collapse="\n")

  paste(
    "---",
    paste0('title: "',rawData$stem, '"'),
    paste0('date: "', format(Sys.time(), "%d-%b-%Y"), '"'),
    paste0("output: html_document"),
    "---\n",
    "```{r}",
    "library(OpenMx)",
    "library(rpf)",
    "",
    paste0(loadData, collapse=""),
    "",
    paste0("if (ncol(data) != ",ncols,") stop('Expecting ",ncols," columns')"),
    paste0("factors <- ", paste0(deparse(fnames)), collapse=""),
    "numFactors <- length(factors)",
    "spec <- list(",
    mkSpec,
    ")",
    "",
    "missingColumns <- which(is.na(match(names(spec), colnames(data))))",
    "if (length(missingColumns)) {",
    "  stop(paste('Columns missing in the data:', omxQuotes(names(spec)[missingColumns])))",
    "}",
    "",
    "#set.seed(1)   # uncomment to get the same starting values every time",
    "startingValues <- mxSimplify2Array(lapply(spec, rpf.rparam))",
    "rownames(startingValues) <- paste('p', 1:nrow(startingValues))",
    "rownames(startingValues)[1:numFactors] <- factors",
    "",
    "imat <- mxMatrix(name='item', values=startingValues, free=!is.na(startingValues))",
    itemInit,
    paste0("m1 <- mxModel(model='",rawData$stem,"', imat,
           mxData(observed=data, type='raw'),
           mxExpectationBA81(ItemSpec=spec),
           mxFitFunctionML())"),
    "",
    paste0("computePlan <- mxComputeSequence(list(mxComputeEM('expectation', 'scores',
                                           mxComputeNewtonRaphson(), verbose=",
           ifelse(input$showFitProgress, "2L", "0L"), "),
                               mxComputeOnce('fitfunction', 'information', 'meat'),
                               mxComputeHessianQuality(),
                               mxComputeStandardError()))"),
    "",
    "m1Fit <- mxRun(mxModel(m1, computePlan))",
    "",
    "m1Grp <- as.IFAgroup(m1Fit, minItemsPerScore=1L)",
    "```",
    sep="\n")
}

# -----------------------------------------------------------------------------------------
shinyServer(function(input, output, session) {
  rawData <- reactiveValues(val=NULL, name=NULL)
  factorData <- reactiveValues(val=NULL)
  itemModel <- reactiveValues()  # colname to list(model, Ta, Tc, startingValues, free, labels, prior)

  observe({
      if (input$exampleDataKCT == 0) return()

      loader <- parse(text=c(
        'utils::data("kct", package="rpf")',
        'data <- kct.people[20:37]',
        'colnames(data) <- mxMakeNames(kct.items[["NAME"]], unique=TRUE)',
        'rownames(data) <- kct.people[[19]]'))
            
      eval(loader)
      rawData$loadDemo <- loader
      rawData$stem <- "kct"
      rawData$val <- data
  })
  
  observe({
    if (input$exampleDataScience == 0) return()
    
    loader <- parse(text=c(
      'utils::data("science", package="rpf")',
      'data <- sfpf[20:44]',
      'rownames(data) <- sfpf[[19]]',
      'colnames(data) <- mxMakeNames(colnames(data), unique=TRUE)',
      'data <- as.data.frame(lapply(data, as.character))'
      ))
    eval(loader)
    
    rawData$loadDemo <- loader
    rawData$stem <- "science"
    rawData$val <- data
  })
  
  observe({
    inFile <- input$file1
    
    if (is.null(inFile)) return(NULL)
    
    if (verbose) {
      cat("read.csv with options", isolate(input$dataRowNames),
          isolate(input$dataSep), isolate(input$dataQuote), fill=T)
    }
    args <- list(inFile$datapath, header=isolate(input$dataHeader),
                 sep=isolate(input$dataSep), quote=isolate(input$dataQuote),
                 stringsAsFactors=FALSE, check.names=FALSE)
    
    if (isolate(input$dataRowNames)) {
      args$row.names=1L
    }
    dat <- try(do.call(read.csv, args), silent = TRUE)
    
    hint <- "Try different quote and separator options."
    validate(need(!inherits(dat, "try-error"),
                  paste("Something went wrong trying to load your file:\n", dat)))
    validate(need(nrow(dat) > 0, paste("Dataset appears to have 0 rows.", hint)),
             need(ncol(dat) > 0, paste("Dataset appears to have 0 columns.", hint)))
    colnames(dat) <- mxMakeNames(colnames(dat), unique=TRUE)
    rawData$name <- inFile$name
    rawData$stem <- sub('\\..{3}$', '', inFile$name, perl=TRUE)
    rawData$val <- dat
  })
  
  output$dataContents <- renderTable({
    dat <- rawData$val
    #    updateTabsetPanel(session, "dataPreviewTabset", "front")  dunno why doesn't work TODO
    updateSelectInput(session, "freqColumnName", selected="-",
                      choices=c("-", colnames(dat)))
    head(dat)
  })
  
  output$nameOfDataFile <- renderText({ rawData$name })

  observe({
    ch <- setdiff(colnames(isolate(rawData$val)), input$freqColumnName)
    updateSelectInput(session, "focusedItem", choices=c(allItemsToken, ch))
  })
  
  output$numberOfDataRows <- renderText({
    nrow(rawData$val)
  })
  
  output$dataSummary <- renderTable({
    dat <- rawData$val
    if (is.null(dat)) return(NULL)
    
    ch <- setdiff(colnames(isolate(rawData$val)), input$freqColumnName)
    tbl <- sapply(rawData$val[,ch], function(col) c(Outcomes=length(unique(col)),
                                                    Missing=sum(is.na(col))))
    tbl <- t(tbl)
    rownames(tbl) <- ch
    tbl
  })
  
  # ------------------------------------------------------------------ Outcomes
  
  
  # ------------------------------------------------------------------ Item Model & Parameters
  
  observe({
    if (is.null(rawData$val)) return()
    
    factorData$val <- as.data.frame(lapply(rawData$val, function(col) {
      mxFactor(col, levels=unique(col), exclude=c(''))
    }))
  })
  
  observe({
    data <- factorData$val
    if (is.null(data)) return()
    
    numFactors <- input$numFactors
    for (fx in 1:numFactors) {
      name <- paste0("nameOfFactor", fx)
      if (isolate(input[[name]]) == "") {
        updateTextInput(session, name, value=sillyFactorName[fx])
      }
    }
    for (col in setdiff(colnames(data), input$freqColumnName)) {
      itemModel[[col]] <- mergeDataAndModel(col, data, numFactors, isolate(itemModel[[col]]))
    }
  })
  
  output$itemModelAssignment <- renderTable({
    if (length(names(itemModel)) == 0) return(NULL)
    
    data <- isolate(factorData$val)
    dcols <- setdiff(colnames(data), input$freqColumnName)
    massign <- lapply(dcols, function (col) {
      im <- itemModel[[col]]
      c(Outcomes=length(levels(data[[col]])), Model=im$model, T.a=im$Ta, T.c=im$Tc)
    })
    names(massign) <- dcols
    t(mxSimplify2Array(massign))
  })
  
  updateItemModel <- observe({
    newModel <- input$focusedItemModel
    fi <- isolate(input$focusedItem)
    if (is.null(newModel) || is.null(fi) || newModel == 'as is' || fi == "No data loaded") return()
    if (verbose) cat("change", fi, "item model to", newModel, fill=T)
    if (fi == allItemsToken) {
      data <- isolate(factorData$val)
      for (col in colnames(data)) {
        changeItemModel(itemModel, input, factorData, col, newModel)
      }
    } else {
      changeItemModel(itemModel, input, factorData, fi, newModel)
    }
  })
  
  observe({
    fi <- input$focusedItem
    if (fi == allItemsToken) {
      sel <- 'as is'
      choices <- c('as is', 'drm', 'grm', 'nrm')
    } else {
      data <- isolate(factorData$val)
      outcomes <- length(levels(data[[fi]]))
      choices <- c('grm')
      if (outcomes == 2) choices <- c('drm', choices)
      if (outcomes > 2) choices <- c(choices, 'nrm')
      sel <- isolate(itemModel[[fi]]$model)
    }
    updateSelectInput(session, "focusedItemModel", choices=choices, selected=sel)
  })
  
  output$itemStartingValuesTable <- renderTable({
    if (length(names(itemModel)) == 0) return(NULL)
    buildParameterTable(input, factorData, itemModel, "starting")
  })
  
  output$itemFreeTable <- renderTable({
    if (length(names(itemModel)) == 0) return(NULL)
    buildParameterTable(input, factorData, itemModel, "free")
  })
  
  output$itemLabelTable <- renderTable({
    if (length(names(itemModel)) == 0) return(NULL)
    buildParameterTable(input, factorData, itemModel, "labels")
  })
  
  output$itemPriorTable <- renderTable({
    if (length(names(itemModel)) == 0) return(NULL)
    buildParameterTable(input, factorData, itemModel, "prior")
  })
  
  observe({
    im <- getFocusedItem(input, itemModel)
    if (is.null(im)) return()
    pname <- getFocusedParameterNames(input, im)
    prevSelect <- match(isolate(input$focusedItemParameter), pname)
    if (is.na(prevSelect)) prevSelect <- 1
    updateSelectInput(session, "focusedItemParameter",
                      choices=pname, selected=pname[prevSelect])
  })
  
  observe({
    im <- getFocusedItem(input, itemModel)
    if (is.null(im)) return()
    fx <- match(input$focusedItemParameter,
                isolate(getFocusedParameterNames(input, im)))
    if (is.na(fx)) return()
    
    spec <- itemToSpec(im)
    if (fx > rpf.numParam(spec)) return()

    choices <- c("Free", 0, 1)
    pi <- rpf.paramInfo(spec, fx)
    if (pi$type == "bound") choices <- c(choices, "inf")
    sel <- computeFreeSelected(im, fx)
    
    allFocused <- input$focusedItem == allItemsToken
    if (allFocused) {
      choices <- c("as is", choices)
      sel <- "as is"
    }
    updateSelectInput(session, "focusedParameterFree", choices=choices,
                      selected=sel)

    label <- im$labels[fx]
    if (is.na(label)) label <- "none"
    if (allFocused) {
      label <- "as is"
    }
    updateTextInput(session, "focusedParameterLabel", value=label)
  })

  observe({
    if (input$focusedParameterFree == "as is") return()
    
    im <- getFocusedItem(input, itemModel)
    if (is.null(im)) return()
    origFx <- isolate(match(input$focusedItemParameter,
                            getFocusedParameterNames(input, im)))
    if (is.na(origFx)) return()
    
    pname <- names(im$starting)[origFx]
    
    allFocused <- isolate(input$focusedItem) == allItemsToken
    if (allFocused) {
      dcols <- isolate(setdiff(colnames(factorData$val), input$freqColumnName))
      for (col in dcols) {
        maybeUpdateFree(input, itemModel, itemModel[[col]], pname)
      }
    } else {
      maybeUpdateFree(input, itemModel, im, pname)
    }
  })
  
  observe({
    hit <- input$changeLabelAction
    
    if (hit == 0 || isolate(input$focusedParameterLabel) == "as is") return()

    im <- isolate(getFocusedItem(input, itemModel))
    if (is.null(im)) return()
    origFx <- isolate(match(input$focusedItemParameter,
                            getFocusedParameterNames(input, im)))
    if (is.na(origFx)) return()

    pname <- names(im$starting)[origFx]
    
    if (isolate(input$focusedItem) == allItemsToken) {
      dcols <- isolate(setdiff(colnames(factorData$val), input$freqColumnName))
      for (col in dcols) {
        maybeUpdateLabel(input, itemModel, itemModel[[col]], pname)
      }
    } else {
      maybeUpdateLabel(input, itemModel, im, pname)
    }
  })
  
  # ------------------------------------------------------------------ Preview & Download
  
  output$debugScriptOutput <- renderText({
    input$debugScriptAction
    isolate(toScript(input, rawData, factorData, itemModel))
  })
  
  output$downloadScript <- downloadHandler(
    filename = function() {
      validate(need(!is.null(rawData$stem), "No data is loaded"))  # TODO, fix
      paste(rawData$stem, '.Rmd', sep='')
    },
    content = function(file) {
      write(isolate(toScript(input, rawData, factorData, itemModel)), file=file)
    }
  )
})
