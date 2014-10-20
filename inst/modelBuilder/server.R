library(shiny)
library(OpenMx)
library(rpf)
library(digest)

# mention citation
# drm == dichotomous help TODO

#options(shiny.trace=TRUE)
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

mergeDataAndModel <- function(col, outcomes, factors, item) {
  if (length(outcomes) != 1) browser()
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

changeItemModel <- function(itemModel, input, outcomes, fi, newModel) {
  oldModel <- isolate(itemModel[[fi]]$model)
  if (is.null(oldModel)) return()
  if (newModel != oldModel) {
    if (verbose) cat("change",fi,"from", oldModel,"to", newModel,fill=T)
    item <- isolate(itemModel[[fi]])
    item$model <- newModel
    numFactors <- isolate(input$numFactors)
    itemModel[[fi]] <- mergeDataAndModel(fi, outcomes, numFactors, item)
  }
}

getFactorNames <- function(input) {
  numFactors <- isolate(input$numFactors)
  sapply(1:numFactors, function (x) input[[ paste0("nameOfFactor", x) ]])
}

buildParameterTable <- function(input, rawData, itemModel, attr) {
  fi <- input$focusedItem
  if (fi == allItemsToken) {
    dcols <- dataColumnNames(input, rawData)
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
  newLabel <- mxMakeNames(isolate(input$focusedParameterLabel))
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

toScript <- function(input, rawData, recodeTable, permuteTable, itemModel) {
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

  data <- rawData$val
  ncols <- ncol(data)
  dcols <- dataColumnNames(input, rawData)
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
    genRecodeOutcomesCode(input, rawData, recodeTable, permuteTable),
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

execRecodeRule <- function(rc, outcomes) {
  if (rc$type == "item") {
    ix <- match(rc$name, names(outcomes))
    if (is.na(ix)) return(outcomes)
    p1 <- setdiff(outcomes[[ix]], rc$from)
    if (rc$to != '') p1 <- union(p1, rc$to)
    outcomes[[ix]] <- sort(p1)
  } else if (rc$type == "outcomeSet") {
    oSetNames <- sapply(outcomes, function(oc) digest(oc, ascii=TRUE))
    for (ix in which(rc$nameHash == oSetNames)) {
      p1 <- setdiff(outcomes[[ix]], rc$from)
      if (rc$to != '') p1 <- union(p1, rc$to)
      outcomes[[ix]] <- sort(p1)
    }
  }
  outcomes
}

recodeOutcomes <- function(input, rawData, recodeTable) {
  dat <- rawData$val
  if (is.null(dat)) return(NULL)
  ch <- dataColumnNames(input, rawData)
  outcomes <- lapply(dat[,ch], function(col) {
    sort(unique(col))
  })
  if (!is.null(recodeTable$val) && nrow(recodeTable$val)) for (rcX in 1:nrow(recodeTable$val)) {
    rc <- recodeTable$val[rcX,]
    outcomes <- execRecodeRule(rc, outcomes)
  }
  outcomes
}

flushFactorTransform <- function(fa1, col) {
  args <- list(col," <- mxFactor(", col, ", ",
               "levels=", paste(deparse(fa1$levels), collapse=""))
  if (any(fa1$levels != fa1$labels)) {
    args <- c(args, ", labels=", paste(deparse(fa1$labels), collapse=""))
  }
  if (length(fa1$exclude)) {
    args <- c(args, ", exclude=", paste(deparse(fa1$exclude), collapse=""))
  }
  args <- c(args, ")")
  do.call(paste0, args)
}

trackRecodeRule <- function(fa1, ix, rc, col) {
  xf1 <- c()
  from <- as.character(rc$from)
  to <- as.character(rc$to)
  if (from != '' && to != '') {
    fx <- match(from, fa1$levels)
    if (is.na(fx)) {
      xf1 <- c(xf1, flushFactorTransform(fa1, paste0("data[[",col,"]]")))
      newLevels <- sort(fa1$labels)
      fa1 <- list(levels=newLevels, labels=newLevels, exclude=c())
    }
    fx <- match(from, fa1$levels)
    fa1$labels[fx] <- to
  } else if (from == '' && to != '') {
    fa1$levels <- append(fa1$levels, to)
    fa1$labels <- append(fa1$labels, to)
  } else if (from != '' && to == '') {
    fx <- match(from, fa1$levels)
    if (is.na(fx)) {
      xf1 <- c(xf1, flushFactorTransform(fa1, paste0("data[[",col,"]]")))
      fa1 <- list(levels=fa1$labels, labels=fa1$labels, exclude=c())
    }
    fa1$exclude <- append(fa1$exclude, from)
    fx <- match(from, fa1$levels)
    fa1$levels <- fa1$levels[-fx]
    fa1$labels <- fa1$labels[-fx]
  }
  list(fa1=fa1, xf1=xf1)
}

genRecodeOutcomesCode <- function(input, rawData, recodeTable, permuteTable) {
  dat <- rawData$val
  if (is.null(dat)) return(NULL)
  ch <- dataColumnNames(input, rawData)
  outcomes <- lapply(dat[,ch], function(col) {
    sort(unique(col))
  })
  farg <- lapply(outcomes, function(lev) list(levels=lev, labels=lev, exclude=c()))

  xform <- c()
  if (!is.null(recodeTable$val) && nrow(recodeTable$val)) for (rcX in 1:nrow(recodeTable$val)) {
    rc <- recodeTable$val[rcX,]
    
    xf1 <- c()
    if (rc$type == "item") {
      ix <- match(rc$name, names(outcomes))
      if (!is.na(ix)) {
        trr <- trackRecodeRule(farg[[ix]], rc$name, rc, paste0("'",rc$name,"'"))
        farg[[ix]] <- trr$fa1
        xf1 <- c(xf1, trr$xf1)
      }
    } else if (rc$type == "outcomeSet") {
      oSetNames <- sapply(outcomes, function(oc) digest(oc, ascii=TRUE))
      xf2 <- c()
      loop <- names(which(rc$nameHash == oSetNames))
      for (ix in loop) {
        trr <- trackRecodeRule(farg[[ix]], ix, rc, "col")
        farg[[ix]] <- trr$fa1
        xf2 <- trr$xf1   # only need 1 copy
      }
      if (length(xf2)) {
        # tricky and hard to test TODO
        xf1 <- c(xf1, paste0("for (col in ",paste("    ", deparse(loop), collapse="\n"),") {\n",
                             xf2, "}\n"))
      }
    }
    xform <- c(xform, xf1)
    outcomes <- execRecodeRule(rc, outcomes)
  }
  
  farg <- lapply(farg, function(fa) {
    perm <- order(fa$labels)
    fa$levels <- fa$levels[perm]
    fa$labels <- fa$labels[perm]
    fa
  })
  
  oSetNames <- sapply(outcomes, function(oc) digest(oc, ascii=TRUE))
  for (pe in names(permuteTable$val)) {
    if (all(pe != oSetNames)) next
    perm <- permuteTable$val[[pe]]
    for (name in names(oSetNames[pe == oSetNames])) {
      fa <- farg[[name]]
      fa$levels <- fa$levels[perm]
      fa$labels <- fa$labels[perm]
      farg[[name]] <- fa
    }
  }
  
  for (pe in intersect(names(permuteTable$val), names(outcomes))) {
    perm <- permuteTable$val[[pe]]
    fa <- farg[[pe]]
    fa$levels <- fa$levels[perm]
    fa$labels <- fa$labels[perm]
    farg[[pe]] <- fa
  }

  fSetNames <- sapply(farg, function(oc) digest(oc, ascii=TRUE))
  fSetTbl <- table(fSetNames)
  if (length(fSetTbl) == 1) {
    xform <- c(xform, flushFactorTransform(farg[[ix]], "data[names(spec)]"))
  } else {
    for (fs in names(fSetTbl)) {
      loop <- names(fSetNames[fs == fSetNames])
      l1 <- loop[1]
      xform <- c(xform, paste0("for (col in ",paste("    ", deparse(loop), collapse="\n"),") {\n  ",
                            flushFactorTransform(farg[[l1]], "data[[col]]"),
                            "\n}"))
      
    }
  }
  
  paste(xform, collapse="\n")
}

getFocusedOutcomeDetailUnordered <- function(input, rawData, recodeTable, permuteTable) {
  outcomes <- recodeOutcomes(input, rawData, recodeTable)  # could accept this as an argument TODO
  if (input$focusedOutcomeItem != '-') {
    fi <- input$focusedOutcomeItem
    iout <- outcomes[[fi]]
    perm <- permuteTable$val[[ digest(iout, ascii=TRUE) ]]
    if (!is.null(perm) && length(perm) == length(iout)) {
      iout <- iout[perm]
    }
    return(iout)
  }
  if (input$focusedOutcomeSet != '-') {
    fi <- input$focusedOutcomeSet
    return(outcomes[[fi]])
  }
  return(NULL)
}

getFocusedOutcomeDetail <- function(input, rawData, recodeTable, permuteTable) {
  outcomes <- getFocusedOutcomeDetailUnordered(input, rawData, recodeTable, permuteTable)
  
  if (input$focusedOutcomeItem != '-') {
    fi <- input$focusedOutcomeItem
  } else if (input$focusedOutcomeSet != '-') {
    fi <- digest(outcomes, ascii=TRUE)
  } else {
    return(outcomes)
  }
  perm <- permuteTable$val[[fi]]
  if (!is.null(perm) && length(perm) == length(outcomes)) {
    outcomes <- outcomes[perm]
  }
  outcomes
}

codingToScript <- function(recodeTable, permuteTable) {
  paste("savedCodingVersion = 1",
        "recodeTable <- ", paste(deparse(recodeTable$val), collapse="\n"),
        "permuteTable <- ", paste(deparse(permuteTable$val), collapse="\n"),
        sep="\n")
}

dataColumnNames <- function(input, rawData) {
  setdiff(colnames(rawData$val), input$freqColumnName)
}

# -----------------------------------------------------------------------------------------
shinyServer(function(input, output, session) {
  feedback <- reactiveValues(newOutcomeAction="", resetRecodeAction="",
                             focusedOutcomeMapAction="", codingFile="")
  rawData <- reactiveValues(val=NULL, name=NULL)
  recodeTable <- reactiveValues(val=NULL)
  permuteTable <- reactiveValues(val=NULL)
  itemModel <- reactiveValues()  # colname to list(model, Ta, Tc, startingValues, free, labels, prior)

  observe({
      if (input$exampleDataKCT == 0) return()

      loader <- parse(text=c(
        'utils::data("kct", package="rpf")',
        'data <- kct.people[20:37]',
        'colnames(data) <- mxMakeNames(kct.items[["NAME"]], unique=TRUE)',
        'data <- as.data.frame(lapply(data, as.character), stringsAsFactors=FALSE)',
        'rownames(data) <- kct.people[[19]]'))

      eval(loader)
      rawData$loadDemo <- loader
      rawData$stem <- "kct"
      rawData$val <- data
      
      recodeTable$val <- 
        structure(list(type = structure(c(1L, 1L, 1L, 1L), .Label = "outcomeSet", class = "factor"), 
                       name = structure(c(1L, 2L, 1L, 1L), .Label = c("X1x1x4", "X18x4x1x3x4x2x1x4"), class = "factor"),
                       nameHash = structure(c(1L,  2L, 3L, 7L), .Label = c("f99c1d57d88bd654baacdcc798a016b6", "8ee3f19dd735a96f0ff59fb514b8efb8", "b695085c9330f407121bb28e859ad508", 
                                                                           "21c943db9d7a66561c37be2e751f7c85", "19eca0e5d319402184b1a1b20eaaa987", 
                                                                           "2572e3e6d229a7405d0ed1839c58f54c", "b867678184d2befa2e1b57c4529dbdc3" ), class = "factor"),
                       action = structure(c(1L, 1L, 2L, 2L), .Label = c("add", "recode"), class = "factor"),
                       from = structure(c(1L, 1L, 2L, 3L), .Label = c("", "0", "1"), class = "factor"), 
                       to = structure(c(1L, 2L, 4L, 3L), .Label = c("0", "1", "correct", "incorrect"), class = "factor")),
                  .Names = c("type", "name", "nameHash", "action", "from", "to"), row.names = c(NA, 4L), class = "data.frame")
      
      permuteTable$val <- 
        structure(list("9b162432e63482dee83b819368c73fb4" = c(2L, 1L)),
                  .Names = "9b162432e63482dee83b819368c73fb4")
  })
  
  observe({
    if (input$exampleDataScience == 0) return()

    loader <- parse(text=c(
      'utils::data("science", package="rpf")',
      'data <- sfpf[20:44]',
      'colnames(data) <- mxMakeNames(colnames(data), unique=TRUE)',
      'data <- as.data.frame(lapply(data, as.character), stringsAsFactors=FALSE)',
      'rownames(data) <- sfpf[[19]]'
    ))
    eval(loader)
    
    rawData$loadDemo <- loader
    rawData$stem <- "science"
    rawData$val <- data
    
    recodeTable$val <- 
      structure(list(type = structure(1L, .Label = "outcomeSet", class = "factor"), 
                     name = structure(1L, .Label = "GOTOMUSEUM", class = "factor"), 
                     nameHash = structure(1L, .Label = "7fcb27479582cc445bad8fa1ce4c4c2a", class = "factor"), 
                     action = structure(1L, .Label = "add", class = "factor"), 
                     from = structure(1L, .Label = "", class = "factor"), to = structure(1L, .Label = "dislike", class = "factor")),
                .Names = c("type", "name", "nameHash", "action", "from", "to"), row.names = 1L, class = "data.frame")
    permuteTable$val <- 
      structure(list("37cba13974a597e56737c53035b1a6f0" = c(1L, 3L, 2L)),
                .Names = "37cba13974a597e56737c53035b1a6f0")
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
    ch <- dataColumnNames(input, rawData)
    updateSelectInput(session, "focusedItem", choices=c(allItemsToken, ch))
  })
  
  output$numberOfDataRows <- renderText({
    nrow(rawData$val)
  })
  
  output$dataSummary <- renderTable({
    dat <- rawData$val
    if (is.null(dat)) return(NULL)
    
    ch <- isolate(dataColumnNames(input, rawData))
    tbl <- sapply(rawData$val[,ch], function(col) c(Outcomes=length(unique(col)),
                                                    Missing=sum(is.na(col))))
    tbl <- t(tbl)
    rownames(tbl) <- ch
    tbl
  })
  
  # ------------------------------------------------------------------ Outcomes
  
  observe({
    prevFocus <- isolate(input$focusedOutcomeSet)
    outcomes <- recodeOutcomes(input, rawData, recodeTable)
    oSetHash <- sapply(outcomes, function(oc) digest(oc, ascii=TRUE))
    ch <- c("-", names(oSetHash[!duplicated(oSetHash)]))
    updateSelectInput(session, "focusedOutcomeSet",
                      choices=ch,
                      selected=ifelse(!is.na(match(prevFocus, ch)), prevFocus, '-'))
  })
  
  observe({
    ch <- dataColumnNames(input, rawData)
    updateSelectInput(session, "focusedOutcomeItem", choices=c("-",ch))
  })
  
  observe({
    if (input$focusedOutcomeItem == '-') return()
    updateSelectInput(session, "focusedOutcomeSet", selected="-")
  })
  
  observe({
    if (input$focusedOutcomeSet == '-') return()
    updateSelectInput(session, "focusedOutcomeItem", selected="-")
  })
  
  output$recodeTable <- renderTable({
    tbl <- recodeTable$val
    if (is.null(tbl) || nrow(tbl) == 0) return()
    tbl[,c("type","name","action","from","to")]
  })

  observe({
    outcomes <- getFocusedOutcomeDetail(input, rawData, recodeTable, permuteTable)
    if (length(outcomes) == 0) outcomes <- "No item selected"
    updateSelectInput(session, "focusedOutcomeMapFrom", choices=outcomes)
    updateSelectInput(session, "focusedOutcomeMapTo", choices=c("<NA>", outcomes, "<Rename>"))
  })
  
  output$addNewOutcomeActionFeedback <- renderText(feedback[["newOutcomeAction"]])

  observe({
    if (input$addNewOutcomeAction == 0) return()
    newOutcome <- isolate(input$newOutcomeName)
    
    if (nchar(newOutcome) == 0) {
      feedback[["newOutcomeAction"]] <- "Enter the name for a new outcome."
      return()
    }

    itemOutcomes <- isolate(getFocusedOutcomeDetailUnordered(input, rawData, recodeTable, permuteTable))

    if (any(newOutcome == itemOutcomes)) {
      feedback[["newOutcomeAction"]] <-
        paste(omxQuotes(newOutcome), "is already an outcome.")
      return()
    }
    
    focusedOutcomeItem <- isolate(input$focusedOutcomeItem)
    focusedOutcomeSet <- isolate(input$focusedOutcomeSet)
    origRecodeTable <- isolate(recodeTable$val)
    
    cat("add outcome", newOutcome, "to item",focusedOutcomeItem,
        "or set",focusedOutcomeSet, fill=T)
    
    if (focusedOutcomeItem != '-') {
      recodeTable$val <-
        rbind(origRecodeTable,
              data.frame(type="item",
                         name=focusedOutcomeItem,
                         nameHash='',
                         action="add",
                         from="",
                         to=newOutcome))
    } else if (focusedOutcomeSet != '-') {
      outcomes <- isolate(recodeOutcomes(input, rawData, recodeTable))
      oSetHash <- sapply(outcomes, function(col) digest(col, ascii=TRUE))
      nameHash <- oSetHash[focusedOutcomeSet == names(oSetHash)][[1]]
      recodeTable$val <-
        rbind(origRecodeTable,
              data.frame(type="outcomeSet",
                         name=focusedOutcomeSet,
                         nameHash=nameHash,
                         action="add",
                         from="",
                         to=newOutcome))
    } else {
      feedback[["newOutcomeAction"]] <- "Select an outcome set or item first."
      return()
    }
    feedback[["newOutcomeAction"]] <- ""
  })
  
  observe({
    # switch to slider? TODO
    if (is.null(recodeTable$val) || nrow(recodeTable$val) == 0) {
      updateNumericInput(session, "focusedRecodeRule", min=1, max=1)
    } else {
      updateNumericInput(session, "focusedRecodeRule", min=1, max=nrow(recodeTable$val))
    }
  })
  
  output$resetRecodeActionFeedback <- renderText(feedback[["resetRecodeAction"]])
  
  observe({
    if (input$resetRecodeAction == 0) return()
    
    origRecodeTable <- isolate(recodeTable$val)
    if (is.null(origRecodeTable) || nrow(origRecodeTable) == 0) {
      feedback[["resetRecodeAction"]] <- "The recode table is empty."
      return()
    }
    
    tbl <- origRecodeTable[-isolate(input$focusedRecodeRule),]
    rownames(tbl) <- NULL
    recodeTable$val <- tbl
    feedback[["resetRecodeAction"]] <- ""
  })
  
  observe({
    if (input$focusedOutcomeMapAction == 0) return()
    
    from <- isolate(input$focusedOutcomeMapFrom)
    to <- isolate(input$focusedOutcomeMapTo)
    if (to == "<Rename>") {
      to <- isolate(input$focusedOutcomeRenameTo)
    }
    if (from == to) {
      feedback[["focusedOutcomeMapAction"]] <-
        paste0("Outcome ",omxQuotes(from)," is already mapped to ",omxQuotes(to),".")
      return()
    }
    if (to == "<NA>") to <- ''
    
    focusedOutcomeItem <- isolate(input$focusedOutcomeItem)
    focusedOutcomeSet <- isolate(input$focusedOutcomeSet)
    origRecodeTable <- isolate(recodeTable$val)

    if (focusedOutcomeItem != '-') {
      recodeTable$val <-
        rbind(origRecodeTable,
              data.frame(type="item",
                         name=focusedOutcomeItem,
                         nameHash='',
                         action="recode",
                         from=from, to=to))
    } else if (focusedOutcomeSet != '-') {
      outcomes <- isolate(recodeOutcomes(input, rawData, recodeTable))
      oSetHash <- sapply(outcomes, function(col) digest(col, ascii=TRUE))
      nameHash <- oSetHash[focusedOutcomeSet == names(oSetHash)][[1]]
      recodeTable$val <-
        rbind(origRecodeTable,
              data.frame(type="outcomeSet",
                         name=focusedOutcomeSet,
                         nameHash=nameHash,
                         action="recode",
                         from=from, to=to))
    } else {
      feedback[["focusedOutcomeMapAction"]] <- "Select an outcome set or item first."
      return()
    }
    feedback[["focusedOutcomeMapAction"]] <- ""
  })

  output$focusedOutcomeMapActionFeedback <- renderText(feedback[["focusedOutcomeMapAction"]])

  output$focusedOutcomeTable <- renderTable(
    data.frame(outcome=getFocusedOutcomeDetail(input, rawData, recodeTable, permuteTable)))
  
  output$reorderOutcomesSorterUI <- renderUI({
    outcomes <- getFocusedOutcomeDetail(input, rawData, recodeTable, permuteTable)
    if (length(outcomes) == 0) {
      return(returnOrder("reorderOutcomesSorter", "Select an item"))
    }
    returnOrder("reorderOutcomesSorter", outcomes)
  })
  
  observe({
    newOrder <- input$reorderOutcomesSorter
    outcomes <- isolate(getFocusedOutcomeDetailUnordered(input, rawData, recodeTable, permuteTable))
    perm <- match(newOrder, outcomes)
    
    focusedOutcomeItem <- isolate(input$focusedOutcomeItem)
    focusedOutcomeSet <- isolate(input$focusedOutcomeSet)
  
    if (focusedOutcomeItem != '-') {
      fi <- focusedOutcomeItem
    } else if (focusedOutcomeSet != '-') {
      fi <- digest(outcomes, ascii=TRUE)
    } else {
      return()
    }
    
    oldPerm <- isolate(permuteTable$val[[fi]])
    if (!is.null(oldPerm) && length(perm) == length(oldPerm) &&
          all(perm == oldPerm)) return()

    if (all(perm == 1:length(perm)) && !is.null(oldPerm)) {
      permuteTable$val[[fi]] <- NULL
    } else if (any(perm != 1:length(perm))) {
      permuteTable$val[[fi]] <- perm
    }
  })
  
  output$permuteTable <- renderTable({
    pt <- permuteTable$val
    if (length(pt) == 0) return()
    t(mxSimplify2Array(pt))
  })
  
  output$downloadCoding <- downloadHandler(
    filename = function() {
      paste(rawData$stem, '-config.R', sep='')
    },
    content = function(file) {
      write(isolate(codingToScript(recodeTable, permuteTable)), file=file)
    }
  )
  
  output$codingFileFeedback <- renderText(feedback[["codingFile"]])

  observe({
    inFile <- input$codingFile
    if (is.null(inFile)) return()
    bubble <- new.env()
    got <- try(source(inFile$datapath, local=bubble, echo=FALSE), silent=TRUE)
    if (inherits(got, "try-error")) {
      feedback[["codingFile"]] <- paste("Parse error", got, sep="\n")
      return()
    }
    if (!exists(envir=bubble, inherits=FALSE, "savedCodingVersion") ||
          get(envir=bubble, inherits=FALSE, "savedCodingVersion") != 1) {
      feedback[["codingFile"]] <- "This does not seem to be a saved coding file."
      return()
    }
    if (!exists(envir=bubble, inherits=FALSE, "recodeTable")) {
      feedback[["codingFile"]] <- "Recoding table is missing"
      return()
    }
    if (!exists(envir=bubble, inherits=FALSE, "permuteTable")) {
      feedback[["codingFile"]] <- "Permutation table is missing"
      return()
    }
    recodeTable$val <- get(envir=bubble, inherits=FALSE, "recodeTable")
    permuteTable$val <- get(envir=bubble, inherits=FALSE, "permuteTable")
    feedback[["codingFile"]] <- ""
  })
  
  # ------------------------------------------------------------------ Item Model & Parameters
  
  observe({
    outcomes <- recodeOutcomes(input, rawData, recodeTable)
    if (is.null(outcomes)) return()
    
    numFactors <- input$numFactors
    for (fx in 1:numFactors) {
      name <- paste0("nameOfFactor", fx)
      fn <- isolate(input[[name]])
      if (is.null(fn) || fn == "") {
        updateTextInput(session, name, value=sillyFactorName[fx])
      }
    }
    for (col in dataColumnNames(input, rawData)) {
      itemModel[[col]] <- mergeDataAndModel(col, length(outcomes[[col]]), numFactors,
                                            isolate(itemModel[[col]]))
    }
  })
  
  output$itemModelAssignment <- renderTable({
    if (length(names(itemModel)) == 0) return(NULL)
    
    outcomes <- recodeOutcomes(input, rawData, recodeTable)
    massign <- mapply(function (col, cname) {
      im <- itemModel[[cname]]
      c(Outcomes=length(col), Model=im$model, T.a=im$Ta, T.c=im$Tc)
    }, outcomes, names(outcomes), SIMPLIFY=FALSE)
    t(mxSimplify2Array(massign))
  })
  
  updateItemModel <- observe({
    newModel <- input$focusedItemModel
    fi <- isolate(input$focusedItem)
    if (is.null(newModel) || is.null(fi) || newModel == 'as is' || fi == "No data loaded") return()
    if (verbose) cat("change", fi, "item model to", newModel, fill=T)
    outcomes <- isolate(recodeOutcomes(input, rawData, recodeTable))
    if (fi == allItemsToken) {
      dcol <- dataColumnNames(input, rawData)
      for (col in dcol) {
        changeItemModel(itemModel, input, length(outcomes[[col]]), col, newModel)
      }
    } else {
      changeItemModel(itemModel, input, length(outcomes[[fi]]), fi, newModel)
    }
  })
  
  observe({
    fi <- input$focusedItem
    if (fi == allItemsToken) {
      sel <- 'as is'
      choices <- c('as is', 'drm', 'grm', 'nrm')
    } else {
      outcomeMap <- recodeOutcomes(input, rawData, recodeTable)
      outcomes <- length(outcomeMap[[fi]])
      choices <- c('grm')
      if (outcomes == 2) choices <- c('drm', choices)
      if (outcomes > 2) choices <- c(choices, 'nrm')
      sel <- isolate(itemModel[[fi]]$model)
    }
    updateSelectInput(session, "focusedItemModel", choices=choices, selected=sel)
  })
  
  output$itemStartingValuesTable <- renderTable({
    if (length(names(itemModel)) == 0) return(NULL)
    buildParameterTable(input, rawData, itemModel, "starting")
  })
  
  output$itemFreeTable <- renderTable({
    if (length(names(itemModel)) == 0) return(NULL)
    buildParameterTable(input, rawData, itemModel, "free")
  })
  
  output$itemLabelTable <- renderTable({
    if (length(names(itemModel)) == 0) return(NULL)
    buildParameterTable(input, rawData, itemModel, "labels")
  })
  
  output$itemPriorTable <- renderTable({
    if (length(names(itemModel)) == 0) return(NULL)
    buildParameterTable(input, rawData, itemModel, "prior")
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
      dcols <- isolate(dataColumnNames(input, rawData))
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
      dcols <- isolate(dataColumnNames(input, rawData))
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
    isolate(toScript(input, rawData, recodeTable, permuteTable, itemModel))
  })
  
  output$downloadScript <- downloadHandler(
    filename = function() {
      validate(need(!is.null(rawData$stem), "No data is loaded"))  # TODO, fix
      paste(rawData$stem, '.Rmd', sep='')
    },
    content = function(file) {
      write(isolate(toScript(input, rawData, recodeTable, permuteTable, itemModel)), file=file)
    }
  )
})
