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

# Function to do drm by groups
drm_groups <- function(params, group, item_stats) {
  tmp <- split(params, params[, group])
  
  tmp_drm <- lapply(1:length(tmp), function(xx) 
    drm(tmp[[xx]][, item_stats], seq(-5, 5, by = .1))@prob)
}

# Function to do iif by groups
iif_groups <- function(params, group, item_stats) {
  tmp <- split(params, params[, group])
  
  tmp_iif <- lapply(1:length(tmp), function(xx) 
    iif(tmp[[xx]][, item_stats], x = seq(-5, 5, by = .01)))
}

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
  
  output$variables <- renderUI({
    vars_id <- names(params())
    selectInput("idvar", "ID Variable", choices = vars_id)
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
    id_var <- input$idvar
     if(nchar(mytext) == 0) {
       mytext <- paste(1:nrow(params()), collapse = ",")
     }
    num_sel <- as.numeric(unlist(strsplit(mytext, ',\\s*')))
    subparams <- params() %>%
      filter_(paste(id_var, '%in% c(', mytext, ')'))
    return(subparams)
  })
  
  paramsA_2 <- eventReactive(input$run2, {
    mytext_2 <- input$selectitems_2
    id_var <- input$idvar
    if(nchar(mytext_2) == 0) {
      mytext_2 <- paste(1:nrow(params()), collapse = ",")
    }
    num_sel <- as.numeric(unlist(strsplit(mytext_2, ',\\s*')))
    subparams <- params() %>%
      filter_(paste(id_var, '%in% c(', mytext_2, ')'))
    return(subparams)
  })
  
  output$ip <- renderDataTable(paramsA())
  
  output$ip2 <- renderDataTable(paramsA_2())
  
  output$avgparams <- renderDataTable({
    if(input$groups == FALSE) {
      avgpar <- paramsA() %>%
        summarise(Form = 'Form 1', Numitems = n(), mean_a = mean(a), 
                  mean_b = mean(b), mean_c = mean(c))
    } else {
      avgpar <- paramsA() %>%
        group_by_(input$groupvar) %>%
        summarise(Numitems = n(), mean_a = mean(a), 
                  mean_b = mean(b), mean_c = mean(c))
    }
    
    if(input$compare == TRUE & input$groups == FALSE) {
      avgpar2 <- paramsA_2() %>%
        summarise(Form = 'Form 2', Numitems = n(), mean_a = mean(a), 
                  mean_b = mean(b), mean_c = mean(c))
      avgpar <- rbind(avgpar, avgpar2)
    } else {
      if(input$compare == TRUE & input$groups == TRUE) {
        avgpar2 <- paramsA_2() %>%
          group_by_(input$groupvar) %>%
          summarise(Numitems = n(), mean_a = mean(a), 
                    mean_b = mean(b), mean_c = mean(c))
        avgpar$Form <- 'Form 1'
        avgpar2$Form <- 'Form 2'
        avgpar <- rbind(avgpar, avgpar2)
        avgpar <- avgpar[c('Form', input$groupvar, 'Numitems', 'mean_a', 
                           'mean_b', 'mean_c')]
      }
    }
    return(avgpar)
  })
  
  output$Vars <- renderUI({
    vars <- names(paramsA())
    selectInput("groupvar", "Grouping Variables", choices = vars)
  })

  tccdat <- reactive({
    if(input$groups == FALSE) {
      params2 <- paramsA() %>%
        select(a, b, c) %>%
        filter(is.na(a) == FALSE)
      params2 <- data.frame(params2)
      names(params2) <- c('a', 'b', 'c')
      
      t1 <- data.frame(drm(params2, seq(-5, 5, by = .1))@prob)
      item_names <- paste("item", paramsA()[, input$idvar], sep = "_")
      colnames(t1) <- c("theta1", item_names)
      t1_names <- paste0(names(t1)[2], ':', names(t1)[ncol(t1)])
      t1 <- t1 %>%
        gather(item, prob, eval(parse(text = t1_names)))
    } else {
      t1 <- drm_groups(paramsA(), input$groupvar, c('a', 'b', 'c'))
      item_names <- unique(paste("item", paramsA()[, input$idvar], sep = "_"))
      t1 <- do.call("rbind", lapply(seq(t1), function(xx) {
        y <- data.frame(unique(paramsA()[, input$groupvar])[xx], t1[[xx]])
        names(y) <- c(input$groupvar, "theta1", item_names)
        return(y)
      })
      )
      t1_names <- paste0(names(t1)[3], ':', names(t1)[ncol(t1)])
      t1 <- t1 %>%
        gather(item, prob, eval(parse(text = t1_names)))
    }
    
  if(input$compare == TRUE & input$groups == FALSE) {
    params3 <- paramsA_2() %>%
      select(a, b, c) %>%
      filter(is.na(a) == FALSE)
    params3 <- data.frame(params3)
    names(params3) <- c('a', 'b', 'c')
    
    t2 <- data.frame(drm(params3, seq(-5, 5, by = .1))@prob)
    item_names <- paste("item", paramsA_2()[, input$idvar], sep = "_")
    colnames(t2) <- c("theta1", item_names)
    t2_names <- paste0(names(t2)[2], ':', names(t2)[ncol(t2)])
    t2 <- t2 %>%
      gather(item, prob, eval(parse(text = t2_names)))
    t2$form <- 'Form 2'
    t1$form <- 'Form 1'
    t1 <- rbind(t1, t2)
  } else {
    if(input$compare == TRUE & input$groups == TRUE) {
      t2 <- drm_groups(paramsA_2(), input$groupvar, c('a', 'b', 'c'))
      item_names <- unique(paste("item", paramsA_2()[, input$idvar], sep = "_"))
      t2 <- do.call("rbind", lapply(seq(t2), function(xx) {
        y <- data.frame(unique(paramsA_2()[, input$groupvar])[xx], t2[[xx]])
        names(y) <- c(input$groupvar, "theta1", item_names)
        return(y)
      })
      )
      t2_names <- paste0(names(t2)[3], ':', names(t2)[ncol(t2)])
      t2 <- t2 %>%
        gather(item, prob, eval(parse(text = t2_names)))
      t2$form <- 'Form 2'
      t1$form <- 'Form 1'
      t1 <- rbind(t1, t2)
    }
  }
    return(t1)
  })

  output$icc1 <- renderPlot({
        # plot TCC for each item
    if(input$compare == FALSE & input$groups == FALSE) {
      f <- ggplot(tccdat(), aes(x = theta1, y = prob, color = factor(item) 
      )) + theme_bw(base_size = 16)
      f <- f + geom_line(size = 1) + 
        scale_color_discrete("Item") + 
        scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                           breaks = seq(0, 1, by = .1)) + 
        scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
        theme(axis.title.y = element_text(vjust = 1.5), 
              axis.title.x = element_text(vjust = -0.25)) + 
        theme(panel.grid.major = element_line(colour = "#a7a7a7"))
    } else {
      if(input$compare == TRUE & input$groups == FALSE) {
        f <- ggplot(tccdat(), aes(x = theta1, y = prob, color = factor(item))) + 
          theme_bw(base_size = 16)
        f <- f + geom_line(size = 1) +
          scale_color_discrete("Item") + 
          scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                             breaks = seq(0, 1, by = .1)) + 
          scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
          theme(axis.title.y = element_text(vjust = 1.5), 
                axis.title.x = element_text(vjust = -0.25)) +
          theme(panel.grid.major = element_line(colour = "#a7a7a7")) +
          facet_grid(. ~ form)
      } else {
        if(input$groups == TRUE & input$compare == FALSE) {
          f <- ggplot(tccdat(), aes(x = theta1, y = prob, color = factor(item))) + 
            theme_bw(base_size = 16)
          f <- f + geom_line(size = 1) +
            scale_color_discrete("Item") + 
            scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                               breaks = seq(0, 1, by = .1)) + 
            scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
            theme(axis.title.y = element_text(vjust = 1.5), 
                  axis.title.x = element_text(vjust = -0.25)) + 
            theme(panel.grid.major = element_line(colour = "#a7a7a7")) +
            facet_grid(reformulate(input$groupvar, "."))
        } else {
          f <- ggplot(tccdat(), aes(x = theta1, y = prob, color = factor(item))) + 
            theme_bw(base_size = 16)
          f <- f + geom_line(size = 1) +
            scale_color_discrete("Item") + 
            scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                               breaks = seq(0, 1, by = .1)) + 
            scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
            theme(axis.title.y = element_text(vjust = 1.5), 
                  axis.title.x = element_text(vjust = -0.25)) + 
            theme(panel.grid.major = element_line(colour = "#a7a7a7")) +
            facet_grid(reformulate(input$groupvar, "form"))
        }
      }
    }
     print(f)
  }, height = 800, width = 1200)
  
  output$iccint <- renderChart2({
    tccdat2 <- tccdat()
    f <- nPlot(y = 'prob', x = 'theta1', group = 'item', data = tccdat2,
               type = 'lineChart')
    f$chart(forceY = c(0, 1))
    f$yAxis(tickValues = seq(0, 1, by = 0.1))
    print(f)
  })
  
  output$tcc <- renderPlot({
    if(input$groups == FALSE) {
      params2_agg <- summarise(paramsA(), mean_a = mean(a), 
                               mean_b = mean(b), mean_c = mean(c))
      t1_agg <- data.frame(drm(params2_agg, seq(-5, 5, by = .01))@prob)
    } else {
      params2_agg <- paramsA() %>%
        group_by_(input$groupvar) %>%
        summarise(mean_a = mean(a), 
                  mean_b = mean(b), mean_c = mean(c)) %>%
        data.frame()
      t1_agg <- do.call("rbind", drm_groups(params2_agg, input$groupvar, 
                           c('mean_a', 'mean_b', 'mean_c')))
      t1_agg[, input$groupvar] <- rep(unique(paramsA()[, input$groupvar]), 
                                      each = 101)
    }
    
    if(input$compare == TRUE & input$groups == FALSE) {
      params3_agg <- summarise(paramsA_2(), mean_a = mean(a), mean_b = mean(b), mean_c = mean(c))
      t2_agg <- data.frame(drm(params3_agg, seq(-5, 5, by = .01))@prob)
      t2_agg$form <- 'Form 2'
      t1_agg$form <- 'Form 1'
      t1_agg <- rbind(t1_agg, t2_agg)
    } else {
      if(input$compare == TRUE & input$groups == TRUE) {
        params3_agg <- paramsA_2() %>%
          group_by_(input$groupvar) %>%
          summarise(mean_a = mean(a), 
                    mean_b = mean(b), mean_c = mean(c)) %>%
          data.frame()
        t2_agg <- do.call("rbind", drm_groups(params3_agg, input$groupvar, 
                                              c('mean_a', 'mean_b', 'mean_c')))
        t2_agg[, input$groupvar] <- rep(unique(paramsA_2()[, input$groupvar]), 
                                        each = 101)
        t2_agg$form <- 'Form 2'
        t1_agg$form <- 'Form 1'
        t1_agg <- rbind(t1_agg, t2_agg)
      }
    }
    
    if(input$compare == FALSE & input$groups == FALSE) {
      f <- ggplot(tccdat(), aes(x = theta1, y = prob, color = factor(item))) + 
        theme_bw(base_size = 16)
      f<- f + geom_line(size = 1, alpha = .5, show_guide = FALSE) +
        #scale_linetype_discrete("Method") + 
        scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                           breaks = seq(0, 1, by = .1)) + 
        scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
        theme(axis.title.y = element_text(vjust = 1.5), 
              axis.title.x = element_text(vjust = -0.25)) + 
        geom_line(data = t1_agg, aes(x = theta1, y = item_1.1), size = 3, color = "black") +
        theme(panel.grid.major = element_line(colour = "#a7a7a7"))
    } else {
      if(input$compare == FALSE & input$groups == TRUE) {
        f <- ggplot(tccdat(), aes(x = theta1, y = prob, color = factor(item))) + 
          theme_bw(base_size = 16)
        f <- f + geom_line(size = 1) +
          scale_color_discrete("Item") + 
          scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                             breaks = seq(0, 1, by = .1)) + 
          scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
          theme(axis.title.y = element_text(vjust = 1.5), 
                axis.title.x = element_text(vjust = -0.25)) +
          geom_line(data = t1_agg, aes(x = theta1, y = item_1.1), size = 3, color = "black") +
          theme(panel.grid.major = element_line(colour = "#a7a7a7")) +
          facet_grid(reformulate(input$groupvar, "."))
      } else {
        if(input$compare == TRUE & input$groups == FALSE) {
          f <- ggplot(tccdat(), aes(x = theta1, y = prob, color = factor(item))) + 
            theme_bw(base_size = 16)
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
        } else {
          f <- ggplot(tccdat(), aes(x = theta1, y = prob, color = factor(item))) + 
            theme_bw(base_size = 16)
          f <- f + geom_line(size = 1, show_guide = FALSE) +
            #scale_linetype_discrete("Method") + 
            scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                               breaks = seq(0, 1, by = .1)) + 
            scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
            theme(axis.title.y = element_text(vjust = 1.5), 
                  axis.title.x = element_text(vjust = -0.25)) + 
            geom_line(data = t1_agg, aes(x = theta1, y = item_1.1), size = 3, color = "black")+ 
            theme(panel.grid.major = element_line(colour = "#a7a7a7")) +
            facet_grid(reformulate(input$groupvar, "form"))
        }
      } 
    }
    
    
    print(f)
  }, height = 800, width = 1200)
  
  output$tccint <- renderChart2({
    #tccdat2 <- tccdat()
    params2_agg <- summarise(paramsA(), mean_a = mean(a), mean_b = mean(b), mean_c = mean(c))
    t1_agg <- data.frame(drm(params2_agg, seq(-5, 5, by = .1))@prob)
    colnames(t1_agg) <- c("theta1", "TCC")
    
    f2 <- nPlot(y = 'TCC', x = 'theta1', data = t1_agg,
               type = 'lineChart')
    f2$chart(forceY = c(0, 1))
    f2$yAxis(tickValues = seq(0, 1, by = 0.1))
    print(f2)
  })
  
  output$tif <- renderPlot({
    if(input$groups == FALSE) {
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
    } else {
      item.inf <- iif_groups(paramsA(), input$groupvar, c('a', 'b', 'c'))
      cinf <- lapply(1:length(item.inf), function(xx) 
        do.call("c", lapply(1:nrow(item.inf[[xx]]$f), function(ii) 
          cumsum(item.inf[[xx]]$f[ii, ]))))
      item.cinf <- do.call("rbind", lapply(1:length(item.inf), function(xx)
        data.frame(ability = rep(seq(-5, 5, by = .01), each = ncol(item.inf[[xx]]$f)),
                   information = cinf[[xx]], id = rep(1:ncol(item.inf[[xx]]$f), times = 1001))))
      item.cinf[, input$groupvar] <- rep(unique(paramsA()[, input$groupvar]), 
                                         each = 1001*length(unique(paramsA()[, input$idvar])))
      item.cinf$group <- ifelse(item.cinf$id == length(unique(paramsA()[, input$idvar])), 1, 0)
    }
    
    if(input$groups == FALSE) {
      f <- ggplot(item.cinf, aes(x = ability, y = information)) + theme_bw(base_size = 16)
      f <- f + geom_line(aes(group = id), color = "gray15", linetype = 2) + 
        scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1)) + 
        scale_y_continuous("Information")+ 
        geom_line(data = subset(item.cinf, group == 1), aes(x = ability, y = information), 
                  size = 1, linetype = 1, color = "black")
    } else {
      f <- ggplot(item.cinf, aes(x = ability, y = information)) + theme_bw(base_size = 16)
      f <- f + geom_line(aes(group = id), color = "gray15", linetype = 2) + 
        scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1)) + 
        scale_y_continuous("Information")+ 
        geom_line(data = subset(item.cinf, group == 1), aes(x = ability, y = information), 
                  size = 1, linetype = 1, color = "black") + 
        facet_grid(reformulate(input$groupvar, "."))
    }
    
    
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
    item_names <- paste("item", arrange(paramsA(), b)$itemnumber,
                        sep = "_")
    item.cinf$id <- rep(item_names, times = 101)
    item.cinf$group <- ifelse(item.cinf$id == nitems, 1, 0)
    
    f3 <- nPlot(y = 'information', x = 'ability', group = "id", data = item.cinf,
               type = 'lineChart')
    f3$chart(forceY = c(0, max(item.cinf$information)))
    f3$yAxis(tickValues = seq(0, max(item.cinf$information), 
                                     length.out = 10))
    print(f3)
  })
  
})
