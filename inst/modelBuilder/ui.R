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
                     '"')
      ),
      mainPanel(
        tabsetPanel(id="dataPreviewTabset",
                    tabPanel("First 6 rows", value="front", tableOutput('dataContents')),
                    tabPanel("Item summary", tableOutput('dataSummary')))
      )
    )
  ),
  tabPanel("Items", titlePanel("Items")),
  tabPanel("Model Fit", titlePanel("Fit")),
  tabPanel("Diagnostics", titlePanel("Diagnostics"))
))
