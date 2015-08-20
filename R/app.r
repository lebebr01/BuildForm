library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)


ui <- fluidPage(
  
  fluidRow(
    column(width = 8,
                 plotOutput('iccint', click = 'plot1_click')
  ),
  column(width = 3,
         verbatimTextOutput("plot_clickinfo")
  )),
  fluidRow(
    column(width = 12,
           dataTableOutput('click_info', width = '80%')
    )
  )
)

server <- function(input, output, session) {
  
  tccdat <- read.csv(file = 'Data/tccdat.csv')
  
  output$iccint <- renderPlot({
    f <- ggplot(tccdat, aes(x = theta1, y = prob)) + # , color = factor(item)
      theme_bw(base_size = 16)
    f <- f + geom_point() +
      scale_color_discrete("Item") + 
      scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                         breaks = seq(0, 1, by = .1)) + 
      scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
      theme(axis.title.y = element_text(vjust = 1.5), 
            axis.title.x = element_text(vjust = -0.25)) + 
      theme(panel.grid.major = element_line(colour = "#a7a7a7"))
    print(f)
  }, height = 400, width = 800)
  
  output$plot_clickinfo <- renderPrint({
    cat("input$plot1_click:\n")
    str(input$plot1_click)
  })
  
  output$click_info <- renderDataTable({
    res <- nearPoints(tccdat, input$plot1_click,xvar = 'theta1', yvar = 'prob',
                      threshold = 100, addDist = TRUE)
#     item_number <- gsub('item_', '', res$item)
#     
#     tmp <- filter_(tccdat, itemnumber %in% item_number)
    datatable(res)
  })
}

shinyApp(ui, server)
