library(shiny)
library(DT)
library(irtoys)
library(plink)
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
library(readxl)
library(rCharts)

options(RCHART_WIDTH = 1200, RCHART_HEIGHT = 800)
shinyServer(function(input, output) {
  
  select <- dplyr::select
  
  params <- reactive({
    if(input$filetype == 1){
      inFile <- input$file1
      
      if(is.null(inFile)) { return(NULL) }
      read.csv(inFile$datapath, header=input$header, sep=input$sep, 
               quote=input$quote)
    } else {
      inFile <- input$file2
      
      if(is.null(inFile)) { return(NULL) }
      file.rename(inFile$datapath,
                  paste(inFile$datapath, ".xlsx", sep=""))
      data.frame(read_excel(paste0(inFile$datapath, '.xlsx'), 1,  
                 col_names = input$colnames))
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
  
  output$selitems2 <- renderText({
    input$selectitems_2
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
  
  paramsA_2 <- eventReactive(input$run2, {
    mytext_2 <- input$selectitems_2
    if(!is.null(mytext_2)) {
      subparams <- params() %>%
        slice(as.numeric(unlist(strsplit(mytext_2, ',\\s*'))))
    } else {
      subparams <- params()
    }
    return(subparams)
  })
  
  output$ip <- renderDataTable(paramsA())
  
  output$ip2 <- renderDataTable(paramsA_2())
  
  output$avgparams <- renderDataTable({
    avgpar <- paramsA() %>%
      summarise(Form = 'Form 1', Numitems = n(), mean_a = mean(a), mean_b = mean(b), mean_c = mean(c))
    if(input$compare == TRUE) {
      avgpar2 <- paramsA_2() %>%
        summarise(Form = 'Form 2', Numitems = n(), mean_a = mean(a), mean_b = mean(b), mean_c = mean(c))
      avgpar <- rbind(avgpar, avgpar2)
    }
    return(avgpar)
  })
  
  output$Vars <- renderUI({
    vars <- names(paramsA())
    selectInput("groups", "Grouping Variables", choices = vars)
  })

  tccdat <- reactive({
    params2 <- paramsA() %>%
      select(a, b, c) %>%
      filter(is.na(a) == FALSE)
    params2 <- data.frame(params2)
    names(params2) <- c('a', 'b', 'c')
    
    t1 <- data.frame(drm(params2, seq(-5, 5, by = .1))@prob)
    t1_names <- paste0(names(t1)[2], ':', names(t1)[ncol(t1)])
    t1 <- t1 %>%
      gather(item, prob, eval(parse(text = t1_names)))
    
  if(input$compare == TRUE) {
    params3 <- paramsA_2() %>%
      select(a, b, c) %>%
      filter(is.na(a) == FALSE)
    params3 <- data.frame(params3)
    names(params3) <- c('a', 'b', 'c')
    
    t2 <- data.frame(drm(params3, seq(-5, 5, by = .1))@prob)
    t2_names <- paste0(names(t2)[2], ':', names(t2)[ncol(t2)])
    t2 <- t2 %>%
      gather(item, prob, eval(parse(text = t2_names)))
    t2$form <- 'Form 2'
    t1$form <- 'Form 1'
    t1 <- rbind(t1, t2)
  }
    return(t1)
  })
  
#   output$icc_plot <- renderUI({
#     plotOutput('icc1', height = 600,
#                click = "plot_click")
#   })
  
  output$icc1 <- renderPlot({
        # plot TCC for each item
        f <- ggplot(tccdat(), aes(x = theta1, y = prob, color = factor(item) 
        )) + theme_bw(base_size = 16)
        f <- f + geom_line(size = 1, show_guide = FALSE) +
          geom_point(size = 1) +
          #scale_linetype_discrete("Method") + 
          scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                             breaks = seq(0, 1, by = .1)) + 
          scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
          theme(axis.title.y = element_text(vjust = 1.5), 
                axis.title.x = element_text(vjust = -0.25)) + 
          #facet_grid(. ~ group)+
          theme(panel.grid.major = element_line(colour = "#a7a7a7"))
        if(input$compare == TRUE) {
          f <- ggplot(tccdat(), aes(x = theta1, y = prob, color = factor(item) 
          )) + theme_bw(base_size = 16)
          f <- f + geom_line(size = 1, show_guide = FALSE) +
            #scale_linetype_discrete("Method") + 
            scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                               breaks = seq(0, 1, by = .1)) + 
            scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
            theme(axis.title.y = element_text(vjust = 1.5), 
                  axis.title.x = element_text(vjust = -0.25)) + 
            #facet_grid(. ~ group)+
            theme(panel.grid.major = element_line(colour = "#a7a7a7")) +
            facet_grid(. ~ form)
        }
        print(f)
  }, height = 800, width = 1200)
  
  output$iccint <- renderChart2({
    tccdat2 <- tccdat()
    f <- nPlot(y = 'prob', x = 'theta1', group = 'item', data = tccdat2,
               type = 'lineChart')
    print(f)
  })
  
#   output$plot_clicked_points <- renderDataTable({
#     dat <- tccdat()
#     res <- nearPoints(dat, input$plot_click, threshold = 15)
#     datatable(res)
#   })
  
  output$tcc <- renderPlot({

    params2_agg <- summarise(paramsA(), mean_a = mean(a), mean_b = mean(b), mean_c = mean(c))
    t1_agg <- data.frame(drm(params2_agg, seq(-5, 5, by = .01))@prob)
    
    f <- ggplot(tccdat(), aes(x = theta1, y = prob, color = factor(item)
    )) + theme_bw(base_size = 16)
    f<- f + geom_line(size = 1, alpha = .5, show_guide = FALSE) +
      #scale_linetype_discrete("Method") + 
      scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                         breaks = seq(0, 1, by = .1)) + 
      scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
      theme(axis.title.y = element_text(vjust = 1.5), 
            axis.title.x = element_text(vjust = -0.25)) + 
      geom_line(data = t1_agg, aes(x = theta1, y = item_1.1), size = 3, color = "black") +
      theme(panel.grid.major = element_line(colour = "#a7a7a7"))
    
    if(input$compare == TRUE) {
      params3_agg <- summarise(paramsA_2(), mean_a = mean(a), mean_b = mean(b), mean_c = mean(c))
      t2_agg <- data.frame(drm(params3_agg, seq(-5, 5, by = .01))@prob)
      t2_agg$form <- 'Form 2'
      t1_agg$form <- 'Form 1'
      t1_agg <- rbind(t1_agg, t2_agg)
      
      f <- ggplot(tccdat(), aes(x = theta1, y = prob, color = factor(item) 
      )) + theme_bw(base_size = 16)
      f <- f + geom_line(size = 1, show_guide = FALSE) +
        #scale_linetype_discrete("Method") + 
        scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                           breaks = seq(0, 1, by = .1)) + 
        scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
        theme(axis.title.y = element_text(vjust = 1.5), 
              axis.title.x = element_text(vjust = -0.25)) + 
        geom_line(data = t1_agg, aes(x = theta1, y = item_1.1), size = 3, color = "black")+ 
        theme(panel.grid.major = element_line(colour = "#a7a7a7")) +
        facet_grid(. ~ form)
    }
    print(f)
  }, height = 800, width = 1200)
  
  output$tccint <- renderChart2({
    #tccdat2 <- tccdat()
    params2_agg <- summarise(paramsA(), mean_a = mean(a), mean_b = mean(b), mean_c = mean(c))
    t1_agg <- data.frame(drm(params2_agg, seq(-5, 5, by = .1))@prob)
    
    f2 <- nPlot(y = 'item_1.1', x = 'theta1', data = t1_agg,
               type = 'lineChart')
    print(f2)
  })
  
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
    
    if(input$compare == TRUE) {
      paramsA_sort_2 <- paramsA_2() %>%
        select(a, b, c) %>%
        arrange(b)
      nitems <- nrow(paramsA_sort_2)
      item.inf_2 <- irtoys::iif(paramsA_sort_2, x = seq(-5, 5, by = .01))
      # plots of individual items by grade - using cumsum across columns of f
      cinf_2 <- do.call("c", lapply(1:nrow(item.inf_2$f), function(xx) cumsum(item.inf_2$f[xx, ])))
      item.cinf_2 <- data.frame(ability = rep(seq(-5, 5, by = .01), each = ncol(item.inf_2$f)),
                              information = cinf_2)
      item.cinf_2$id <- rep(1:ncol(item.inf_2$f), times = 1001)
      item.cinf_2$group <- ifelse(item.cinf_2$id == nitems, 1, 0)
      item.cinf_2$form <- 'Form 2'
      item.cinf$form <-'Form 1'
      item.cinf <- rbind(item.cinf, item.cinf_2)
      
      f <- ggplot(item.cinf, aes(x = ability, y = information)) + theme_bw(base_size = 16)
      f <- f + geom_line(aes(group = id), color = "gray15", linetype = 2) + 
        scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1)) + 
        scale_y_continuous("Information")+ 
        geom_line(data = subset(item.cinf, group == 1), aes(x = ability, y = information), size = 1, linetype = 1, color = "black") + 
        facet_grid(. ~ form)
      
    }
    print(f)
  }, height = 800, width = 1200)
  
  output$tifint <- renderChart2({
    paramsA_sort <- paramsA() %>%
      select(a, b, c) %>%
      arrange(b)
    nitems <- nrow(paramsA_sort)
    item.inf <- irtoys::iif(paramsA_sort, x = seq(-5, 5, by = .1))
    # plots of individual items by grade - using cumsum across columns of f
    cinf <- do.call("c", lapply(1:nrow(item.inf$f), function(xx) cumsum(item.inf$f[xx, ])))
    item.cinf <- data.frame(ability = rep(seq(-5, 5, by = .1), each = ncol(item.inf$f)),
                            information = cinf)
    item.cinf$id <- rep(1:ncol(item.inf$f), times = 101)
    item.cinf$group <- ifelse(item.cinf$id == nitems, 1, 0)
    
    f3 <- nPlot(y = 'information', x = 'ability', group = "id", data = item.cinf,
               type = 'lineChart')
    print(f3)
  })
  
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