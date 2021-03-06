library(shiny)
library(DT)
library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Setup", tabName = "setup", icon = icon('cog')),
    menuItem("Item Parameters", tabName = 'itempar', icon = icon('table')),
    menuItem("Item Characteristic Curve", tabName = 'icc', icon = icon('line-chart')),
    menuItem("Test Characteristic Curve", tabName = 'tcc', icon = icon('line-chart')),
    menuItem('TCC Combined', tabName = 'tcccombtab', icon = icon('line-chart')),
    menuItem('Test Information', tabName = 'tif', icon = icon('line-chart'))
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
   tabItem(tabName = 'setup',
           fluidRow(
             h1('Item Setup'),
             box(
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
             box(
               h3('Select ID Variable'),
               hr(),
               uiOutput('variables'),
               h5('Select the variable to identify items. This will be used
                  to select items included in the analysis'),
               hr()
             ),
             box(
               h3('Select IRT Item Parameter Variables'),
               hr(),
               uiOutput('param_v'),
               h5('Select the variables that correspond to the discrimination (a),
                  difficulty (b), and pseudo-guessing (c) parameters.'),
               h6('These must be specified in the order of a, then b, then c. Note: Only the 
                  b parameter is needed for the equivalent 1PL model.'),
               hr()
               ),
             box(
               h3('Select Items Form 1'),
               hr(),
               uiOutput('items'),
               actionButton('run', 'Update'),
               h5('Use the text box above to select items to include in the analysis.'),
               h6('Note: Click update button to run analysis (even if no items are selected in box above).
                  If no items identified in the text box, analysis is run on all items.')
             )
             ),
           # fluidRow(
           #   
           # ),
           fluidRow(
             box(
               width = 4,
               checkboxInput('compare', label = 'Compare Forms?', value = FALSE),
               hr(),
               conditionalPanel(
                 condition = "input.compare == true",
                 h3('Build Form 2'),
                 hr(),
                 uiOutput('items_form2'),
                 actionButton('run2', 'Update Form 2'),
                 h5('Use the text box above to select items to include in analysis for Form 2, each item must be separated by a comma.'),
                 h5('Note: Click update button to run comparison analysis.'),
                 hr(),
                 hr()
               )
             ),
             box(
               width = 4,
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
             box(
               width = 4,
               checkboxInput('filter', label = 'Filter Data?',
                             value = FALSE),
               hr(),
               conditionalPanel(
                 condition = 'input.filter == true',
                 h2('Select Filter Variable(s)'),
                 hr(),
                 uiOutput('filtervars'),
                 hr(),
                 uiOutput('filter_2'),
                 radioButtons('filter_type', 'Type of Filtering',
                              choices = list(
                                'Or' = '|', 'And' = '&'
                              ), 
                              selected = '|'),
                 h5('The data will be filtered based on the variables
                           and values entered'),
                 h5('Note: Or is an expanding operator, And is a 
                           restricting operator when filtering data'),
                 hr()
               )
             )
           ),
           fluidRow(
             box(
               width = 4,
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
             )
           )
           ),
   tabItem(tabName = 'itempar',
           fluidRow(
             box(
               width = 12,
               h4('Form 1 IRT Parameters'),
               dataTableOutput('ip')
             )
           ),
           fluidRow(
             box(
               width = 12,
               conditionalPanel(
                 condition = "input.compare == true",
                 h4('Form 2 IRT Parameters'),
                 dataTableOutput('ip2')
               )
             )
           ),
           fluidRow(
             box(
               width = 12,
               h4('Average IRT Parameters'),
               dataTableOutput('avgparams', width = '80%')
             )
           )
   ),
   tabItem(tabName = 'icc',
           fluidRow(
             box(
               width = 12,
               plotOutput('icc1', click = 'plot1_click', height = 600)
             )
           ),
           fluidRow(
             box(
               width = 12,
               wellPanel(
                 dataTableOutput('click_info', width = '80%')
               )
             )
           )
   ),
   tabItem(tabName = 'tcc',
           fluidRow(
             box(
               width = 12,
               plotOutput('tcc', click = 'click_tcc', height = 600)
             )
           ),
           fluidRow(
             box(
               width = 12,
               wellPanel(
                 dataTableOutput('click_tcc_info', width = '80%')
               )
             )
           )
   ),
   tabItem(tabName = 'tcccombtab',
           fluidRow(
             box(
               width = 12,
               plotOutput('tcc_comb', click = 'click_tcc_comb', height = 600)
             )
           ),
           fluidRow(
             box(
               width = 12,
               wellPanel(
                 dataTableOutput('click_tcc_comb_info', width = '80%')
               )
             )
           )
   ),
   tabItem(tabName = 'tif',
           fluidRow(
             box(
               width = 12,
               plotOutput('tif', click = 'click_tif', height = 600)
             )
           ),
           fluidRow(
             box(
               width = 12,
               wellPanel(
                 dataTableOutput('click_tif', width = '80%')
               )
             )
           )
   )
  )
)

shinyUI(
  dashboardPage(skin = "green",
  dashboardHeader(title = 'Form Building'),
  sidebar,
  body
 )
)
