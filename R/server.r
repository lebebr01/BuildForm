library(shiny)
library(DT)
library(plink)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggvis)
library(knitr)
library(readxl)

shinyServer(function(input, output) {

  params <- reactive({
    if(input$filetype == 1){
          inFile <- input$file1
          
          if(is.null(inFile)) { return(NULL) }
      read.csv(inFile$datapath, header=input$header, sep=input$sep, 
               quote=input$quote)
    } else {
      inFile <- input$file2
      
      if(is.null(inFile)) { return(NULL) }
      read_excel(inFile$datapath, sheet = input$sheet, 
                 col_names = input$colnames)
    }
    
    })
  
  output$items <- renderUI({
    textInput('selectitems', 'Select Items')
  })
  
  output$items_form2 <- renderUI({
    textInput('selectitems_2', 'Select Items Form 2')
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
  
  output$avgparams <- renderDataTable({
    paramsA() %>%
      summarise(numitems = n(), mean_a = mean(a), mean_b = mean(b), mean_c = mean(c))
  })
  
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
  
  output$tif <- renderPlot({
    paramsA_sort <- paramsA() %>%
      select(a, b, c) %>%
      arrange(b)
    nitems <- nrow(paramsA_sort)
    item.inf <- irtoys::iif(paramsA_sort, x = seq(-5, 5, by = .01))
    # plots of individual items by grade - using cumsum across columns of f
    cinf <- do.call("c", lapply(1:nrow(item.inf$f), function(xx) cumsum(item.inf$f[xx, ])))
    item.cinf <- data.frame(ability = rep(seq(-5, 5, by = .01), each = ncol(item.inf$f)),
                            information = cinf)
    item.cinf$id <- rep(1:ncol(item.inf$f), times = 1001)
    item.cinf$group <- ifelse(item.cinf$id == nitems, 1, 0)
    
    f <- ggplot(item.cinf, aes(x = ability, y = information)) + theme_bw(base_size = 16)
    f <- f + geom_line(aes(group = id), color = "gray15", linetype = 2) + 
      scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1)) + 
      scale_y_continuous("Information")+ 
      geom_line(data = subset(item.cinf, group == 1), aes(x = ability, y = information), size = 1, linetype = 1, color = "black")
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