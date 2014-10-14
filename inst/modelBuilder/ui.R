library(shiny)

shinyUI(navbarPage(
  "OpenMx IFA Model Builder",
  tabPanel(
    "Data",
    sidebarLayout(
      sidebarPanel(
        fileInput('file1', 'Choose CSV File',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        tags$hr(),
        checkboxInput('dataHeader', 'Header?', TRUE),
        checkboxInput('dataRowNames', 'Row names?', TRUE),
        radioButtons('dataSep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        radioButtons('dataQuote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"'),
        conditionalPanel('length(input.nameOfDataFile) == 0',  # TODO fix
                         tags$hr(),
                         helpText("Load example data"),
                         actionButton("exampleDataKCT", label = "KCT"),
                         actionButton("exampleDataScience", label = "Science"))
      ),
      mainPanel(
        tabsetPanel(id="dataPreviewTabset",
                    tabPanel("First 6 rows", value="front",
                             tags$p("Data file:"),
                             verbatimTextOutput("nameOfDataFile"),
                             tags$p("Number of rows:"),
                             verbatimTextOutput("numberOfDataRows"),
                             hr(),
                             tableOutput('dataContents')),
                    tabPanel("Item summary",
                             selectInput("freqColumnName", label = "Row frequency column:",
                                         choices="No data loaded"),
                             hr(),
                             tableOutput('dataSummary')))
      )
    )
  ),
  tabPanel("Outcomes"
           # need special widget for this TODO
           # exclude list for mxFactor
           # collapse list
           # add categories not in the data
           ),
  tabPanel("Model",
           sidebarLayout(
             sidebarPanel(
               selectInput("focusedItem", label = "Edit item:",
                           choices="No data loaded"),
               selectInput("focusedItemModel", label = "Model:",
                           choices=c('drm', 'grm', 'nrm')),
               selectInput("focusedItemParameter", label = "Parameter:",
                           choices="No item selected"),
               selectInput("focusedParameterFree", label = "Free",
                           choices="No parameter selected"),
               textInput("focusedParameterLabel", label = "Label"),
               actionButton("changeLabelAction", label = "Set Label"),
               conditionalPanel('input.focusedItemModel == "drm"',
                                selectInput("focusedParameterPrior", label = "Prior",
                                            "logit normal", "beta"))
             ),
             mainPanel(tabsetPanel(
               tabPanel("Factors",
                        sliderInput("numFactors", "Number of factors:", 
                                    min=1, max=5, value=1),
                        textInput("nameOfFactor1", "Factor 1"),
                        textInput("nameOfFactor2", "Factor 2"),
                        textInput("nameOfFactor3", "Factor 3"),
                        textInput("nameOfFactor4", "Factor 4"),
                        textInput("nameOfFactor5", "Factor 5")),
               tabPanel("Model", tableOutput('itemModelAssignment')),
               tabPanel("Parameters",
                        helpText("Starting values"),
                        tableOutput('itemStartingValuesTable'),
                        helpText("Is free?"),
                        tableOutput('itemFreeTable'),
                        helpText("Labels"),
                        tableOutput('itemLabelTable'),
                        helpText("Bayesian prior"),
                        tableOutput('itemPriorTable'))
               ))
           )),
  tabPanel("Preview",
           actionButton("debugScriptAction", label = "Refresh!"),
           verbatimTextOutput("debugScriptOutput")),
  tabPanel("Download", sidebarLayout(
    sidebarPanel(
      checkboxInput("showFitProgress", label = "Show model fitting progress", value = TRUE),
      checkboxInput("fitReferenceModels", label = "Fit reference models (for more fit statistics)",
                    value = FALSE),
      selectInput("infoMethod", "Information matrix method:", 
                  choices = c("Oakes (1999)", "Meat", "Agile SEM", "*none*")),
      downloadButton('downloadScript', 'Download')
    ),
    mainPanel(
      withTags(ol(
        li('Download your analysis script.'),
        li('Open it in RStudio.'),
        li('Update the pathname to your data (if necessary).'),
        li('Click the Knit/HTML button at the top of your document.')
      )),
      withTags(p(br(),br(),br(),br(),br(),br(),br(),br(),br()))
    )
  ))

))
