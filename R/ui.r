library(shiny)
library(DT)
library(shinythemes)
library(rCharts)

shinyUI(navbarPage(theme = shinytheme("journal"),
  title = 'Form Building',
  tabPanel('Input File', 
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
                  h2('Select ID Variable'),
                  hr(),
                  uiOutput('variables'),
                  h5('Select the variable to identify items. This will be used
                     to select items included in the analysis')
                  ),
           column(4, 
                  h2('Select Items Form 1'),
                  hr(),
                  uiOutput('items'),
                  actionButton('run', 'Update'),
                  h5('Use the text box above to select items to include in the analysis, each item must be separated by a comma.'),
                  h5('Note: Click update button to run analysis (even if no items are selected in box above).'),
                  h5('\n If no items identified in the text box, analysis is run on all items.')
                  )
           )
           )
  ),
  tabPanel('Advanced Selection',
           fluidPage(
             fluidRow(
               column(4, 
                      checkboxInput('compare', label = 'Compare Forms?', value = FALSE),
                      hr(),
                      conditionalPanel(
                        condition = "input.compare == true",
                        h2('Build Form 2'),
                        hr(),
                        uiOutput('items_form2'),
                        actionButton('run2', 'Update Form 2'),
                        h5('Use the text box above to select items to include in analysis for Form 2, each item must be separated by a comma.'),
                        h5('Note: Click update button to run comparison analysis.'),
                        hr(),
                        hr()
                      ),
                      checkboxInput('download', label = 'Download Data?', value = FALSE),
                      hr(),
                      conditionalPanel(
                        condition = 'input.download == true',
                        h2('Select Data to Download'),
                        hr(),
                        selectizeInput('dataset', 'Choose Dataset',
                                       choices = c('Form 1', 'Form 2'),
                                       multiple = TRUE),
                        downloadButton('downloadData', 'Download'),
                        h5('Select dataset(s) to download to file. Will only include items
                           that are shown in the "item parameters" tab for selected forms.'),
                        hr()
                      )
               ),
               column(4, 
                      checkboxInput('groups', label = 'Compare by Groups?', 
                                    value = FALSE),
                      hr(),
                      conditionalPanel(
                        condition = 'input.groups == true',
                        h2('Select Grouping Variable'),
                        hr(),
                        uiOutput('Vars'),
                        h5('This will condition the analysis based on the group
                           variable selected.'),
                        hr()
                      )
               ),
               column(4, 
                      checkboxInput('filter', label = 'Filter Data?',
                                    value = FALSE),
                      hr(),
                      conditionalPanel(
                        condition = 'input.filter == true',
                        h2('Select Filter Variable(s)'),
                        hr(),
                        uiOutput('filtervars'),
                        h5('The data will be filtered based on the variables
                           and values entered'),
                        hr()
                      )
               )
             )
           )
  ),
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
  tabPanel('Item Characteristic Curves', 
           plotOutput('icc1', click = 'plot1_click', height = 600),
           wellPanel(
             dataTableOutput('click_info', width = '80%')
           )
  ),
  navbarMenu("Test Characteristic Curve",
    tabPanel('TCC - with ICC', 
             plotOutput('tcc', click = 'click_tcc', height = 600),
             wellPanel(
               dataTableOutput('click_tcc_info', width = '80%')
             )
    ),
    tabPanel('TCC One Plot',
             plotOutput('tcc_comb', click = 'click_tcc_comb', height = 600),
             wellPanel(
               dataTableOutput('click_tcc_comb_info', width = '80%')
             ))
  ),
  
  tabPanel('Test Information Function', 
           plotOutput('tif', click = 'click_tif', height = 600),
           wellPanel(
             dataTableOutput('click_tif', width = '80%')
           ))
))
