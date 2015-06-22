library(shiny)
library(DT)
library(ggvis)
library(shinythemes)

shinyUI(navbarPage(theme = shinytheme("journal"),
  title = 'Form Building',
  tabPanel('Select File', 
           fluidPage(
           fluidRow(
             column(3,
                    h2('Load Item Parameters'),
                    hr(),
                    radioButtons('filetype', label = 'Type of File',
                                 choices = list('text' = 1, 'excel' = 2)),
                    hr(),
                    conditionalPanel(
                      condition = "input.filetype == 2",
                      fileInput('file2', 'Choose excel File',
                                accept = c('.xls', '.xlsx')),
                      hr(),
                      checkboxInput('colnames', 'Variable Names', TRUE),
                      textInput('sheet', 'Excel Sheet Name to Load')
                    ),
                    conditionalPanel(
                      condition = "input.filetype == 1",
                      fileInput('file1', 'Choose CSV File',
                                accept=c('text/csv', 
                                         'text/comma-separated-values,text/plain', 
                                         '.csv')),
                      hr(),
                      checkboxInput('header', 'Variable Names', TRUE),
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
                    )
             
           ),
           column(4,
                  h2('Build Form 1'),
                  hr(),
                  uiOutput('items'),
                  actionButton('run', 'Update'),
                  h5('Use the text box above to select items to include in the analysis, each item must be separated by a comma.'),
                  h5('Note: Click update button to run analysis (even if no items are selected in box above).'),
                  h5('\n If no items identified in the text box, analysis is run on all items.')),
           column(4, 
                  checkboxInput('compare', label = 'Compare Forms', value = FALSE),
                  hr(),
                  conditionalPanel(
                    condition = "input.compare == true",
                    h2('Build Form 2'),
                    hr(),
                    uiOutput('items_form2'),
                    actionButton('run2', 'Update Form 2'),
                    h5('Use the text box above to select items to include in analysis for Form 2, each item must be separated by a comma.'),
                    h5('Note: Click update button to run comparison analysis.')
                  ))
           ))),
  tabPanel('Item Parameters',  
           h4('Item by Item IRT Parameters'),
           dataTableOutput('ip'), 
           br(), 
           conditionalPanel(
             condition = "input.compare == true",
             h4('Form 2 IRT Parameters'),
             dataTableOutput('ip2')
           ),
           hr(),
           h4('Average IRT Parameters'),
           dataTableOutput('avgparams', width = '80%')),
  tabPanel('Item Characteristic Curves', plotOutput('icc1')),
  tabPanel('Test Characteristic Curve',  plotOutput('tcc')),
  tabPanel('Test Information Function', plotOutput('tif'))
  ))
