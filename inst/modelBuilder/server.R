library(shiny)

#options(shiny.reactlog=TRUE)

verbose <- TRUE

shinyServer(function(input, output, session) {
  state <- reactiveValues(data=NULL)

  output$dataContents <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile)) return(NULL)
    
    if (verbose) {
      cat("read.csv with options", isolate(input$dataRowNames),
          isolate(input$dataSep), isolate(input$dataQuote), fill=T)
    }
    args <- list(inFile$datapath, header=isolate(input$dataHeader),
                 sep=isolate(input$dataSep), quote=isolate(input$dataQuote), stringsAsFactors=FALSE)
    
    if (isolate(input$dataRowNames)) {
      args$row.names=1L
    }
    dat <- try(do.call(read.csv, args), silent = TRUE)
  
    hint <- "Try different quote and separator options."
    validate(need(!inherits(dat, "try-error"),
                  paste("Something went wrong trying to load your file:\n", dat)))
    validate(need(nrow(dat) > 0, paste("Dataset appears to have 0 rows.", hint)),
             need(ncol(dat) > 0, paste("Dataset appears to have 0 columns.", hint)))
    state$data <- dat
#    updateTabsetPanel(session, "dataPreviewTabset", "front")  dunno why doesn't work TODO
    head(dat)
  })
  
  output$dataSummary <- renderTable({
    dat <- state$data
    if (is.null(dat)) return(NULL)
    
    df <- data.frame(Item=colnames(dat),
               Categories=sapply(dat, function(col) length(table(col))),
               Missing=sapply(dat, function(col) sum(is.na(col))))
    rownames(df) <- NULL
    df
  })
})
