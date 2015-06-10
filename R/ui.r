library(shiny)
library(DT)
library(ggvis)

shinyUI(navbarPage(
  title = 'Form Building',
  tabPanel('Select File', 
           fluidPage(
           fluidRow(
             column(4,
             fileInput('file1', 'Choose CSV File',
                              accept=c('text/csv', 
                                       'text/comma-separated-values,text/plain', 
                                       '.csv')),
                    tags$hr(),
                    checkboxInput('header', 'Header', TRUE),
                    radioButtons('sep', 'Separator',
                                 c(Comma=',',
                                   Semicolon=';',
                                   Tab='\t'),
                                 ','),
                    radioButtons('quote', 'Quote',
                                 c(None='',
                                   'Double Quote'='"',
                                   'Single Quote'="'"),
                                 '"')
           ),
           column(4,
                  uiOutput('items'),
                  actionButton('run', 'Update'),
                  h4('Use the text box above to select items to include in analysis, each item must be separated by a comma.'),
                  h4('Note: Click update button to run analysis after loading file.'),
                  h4('\n If no items identified in the text box, analysis is run on all items.'))))),
  tabPanel('Item Parameters',     dataTableOutput('ip')),
  tabPanel('Item Characteristic Curves', plotOutput('icc1')),
  tabPanel('Test Characteristic Curve', plotOutput('tcc'))
))
