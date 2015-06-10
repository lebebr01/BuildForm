library(shiny)
library(DT)
library(plink)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggvis)

shinyServer(function(input, output) {

  params <- reactive({
    inFile <- input$file1
    
    if(is.null(inFile)) { return(NULL) }
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
    })
  
  output$items <- renderUI({
    textInput('selectitems', 'Select Items')
  })
  
  output$selitems <- renderText({
    input$selectitems
  })
  
  paramsA <- eventReactive(input$run, {
    mytext <- input$selectitems
     if(!is.null(mytext)) {
       subparams <- params() %>%
         slice(as.numeric(unlist(strsplit(mytext, ',\\s*'))))
     } else {
       subparams <- params()
     }
    return(subparams)
  })
  
  
  
  output$ip <- renderDataTable(paramsA())
  
  output$icc1 <- renderPlot({
        params2 <- paramsA() %>%
          select(a, b, c) %>%
          filter(is.na(a) == FALSE)
        params2 <- data.frame(params2)
        names(params2) <- c('a', 'b', 'c')
        
        t1 <- data.frame(drm(params2, seq(-5, 5, by = .1))@prob)
        t1_names <- paste0(names(t1)[2], ':', names(t1)[ncol(t1)])
        t1 <- t1 %>%
          gather(item, prob, eval(parse(text = t1_names)))
        
        # plot TCC for each item
        f <- ggplot(t1, aes(x = theta1, y = prob, color = factor(item) 
        )) + theme_bw(base_size = 16)
        f <- f + geom_line(size = 1, show_guide = FALSE) +
          #scale_linetype_discrete("Method") + 
          scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                             breaks = seq(0, 1, by = .1)) + 
          scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
          theme(axis.title.y = element_text(vjust = 1.5), 
                axis.title.x = element_text(vjust = -0.25)) + 
          #facet_grid(. ~ group)+
          theme(panel.grid.major = element_line(colour = "#a7a7a7"))
        print(f)
  }, height = 800, width = 1200)
  
  output$tcc <- renderPlot({
    params2 <- paramsA() %>%
      select(a, b, c) %>%
      filter(is.na(a) == FALSE)
    params2 <- data.frame(params2)
    names(params2) <- c('a', 'b', 'c')
    
    t1 <- data.frame(drm(params2, seq(-5, 5, by = .1))@prob)
    t1_names <- paste0(names(t1)[2], ':', names(t1)[ncol(t1)])
    t1 <- t1 %>%
      gather(item, prob, eval(parse(text = t1_names)))
    
    params2_agg <- summarise(params(), mean_a = mean(a), mean_b = mean(b), mean_c = mean(c))
    t1_agg <- data.frame(drm(params2_agg, seq(-5, 5, by = .01))@prob)
    f <- ggplot(t1, aes(x = theta1, y = prob, color = factor(item) 
    )) + theme_bw(base_size = 16)
    f<- f + geom_line(size = 1, alpha = .5, show_guide = FALSE) +
      #scale_linetype_discrete("Method") + 
      scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                         breaks = seq(0, 1, by = .1)) + 
      scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
      theme(axis.title.y = element_text(vjust = 1.5), 
            axis.title.x = element_text(vjust = -0.25)) + 
      geom_line(data = t1_agg, aes(x = theta1, y = item_1.1), size = 3, color = "black") +
      #facet_grid(. ~ group)+
      theme(panel.grid.major = element_line(colour = "#a7a7a7"))
    print(f)
  }, height = 800, width = 1200)
  
#   icc <- reactive({
#     params2 <- params() %>%
#       select(a, b, c) %>%
#       filter(is.na(a) == FALSE)
#     params2 <- data.frame(params2)
#     names(params2) <- c('a', 'b', 'c')
#     
#     t1 <- data.frame(drm(params2, seq(-5, 5, by = .1))@prob)
#     t1 <- t1 %>%
#       gather(item, prob, item_1.1:item_10.1)
#     
#     t1 %>% ggvis(x = theta1, y = prob, stroke = ~item) %>%
#       layer_lines()
#   })
#   
#   icc %>% bind_shiny('icc1')
  
})