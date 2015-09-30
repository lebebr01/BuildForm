library(shiny)
library(DT)
library(plink)
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
library(readxl)
library(readr)
library(grid)

# Function to compute item information
item_inf <- function(params, ability) {
  p <- drm(params, ability)@prob
  p_ability <- p[, 1]
  p_inf <- p[, 2:ncol(p)]

  if(ncol(params) == 1) {
    iif <- p_inf * (1 - p_inf)
  } else {
    if(ncol(params) == 2) {
      iif <- sweep(p_inf * (1 - p_inf), 2, params[, 1]^2, '*') 
    } else {
      q <- drm(params[, 1:2], ability)@prob
      q_inf <- q[, 2:ncol(q)]
      iif <- sweep(q_inf^2 * (1 - p_inf)/p_inf, 2, params[, 1]^2, '*') 
    }
  }
  return(as.matrix(iif))
}

# Function to do drm by groups
drm_groups <- function(params, group, item_stats) {
  tmp <- split(params, params[, group])
  
  tmp_drm <- lapply(1:length(tmp), function(xx) 
    drm(tmp[[xx]][, item_stats], seq(-5, 5, by = .01))@prob)
  return(tmp_drm)
}

# Function to do iif by groups
iif_groups <- function(params, group, item_stats) {
  tmp <- split(params, params[, group])
  
  tmp_iif <- lapply(1:length(tmp), function(xx)
    item_inf(tmp[[xx]][, item_stats], ability = seq(-5, 5, by = .01)))
  return(tmp_iif)
}

options(RCHART_WIDTH = 1200, RCHART_HEIGHT = 800, 
        useFancyQuotes = FALSE)
shinyServer(function(input, output, session) {
  
  select <- dplyr::select
  
  params <- reactive({
    if(input$filetype == 1){
      inFile <- input$file1
      
      if(is.null(inFile)) { return(NULL) }
      tmp <- read_delim(inFile$datapath, col_names=input$header, delim=input$sep, 
               quote=input$quote)
    } else {
      inFile <- input$file2
      
      if(is.null(inFile)) { return(NULL) }
      file.rename(inFile$datapath,
                  paste(inFile$datapath, ".xlsx", sep=""))
      tmp <- read_excel(paste0(inFile$datapath, '.xlsx'), 1,  
                            col_names = input$colnames)
    }
    names(tmp) <- gsub("\\s+", "_", names(tmp))
    return(tmp)
  })
  
  output$variables <- renderUI({
    vars_id <- names(params())
    selectInput("idvar", "ID Variable", choices = vars_id)
  })
  
  output$param_v <- renderUI({
    vars_id <- names(params())
    selectInput("param_vals", "IRT Parameter Values", choices = vars_id,
                multiple = TRUE)
  })
  
  output$items <- renderUI({
    it <- unique(params()[, input$idvar])
    selectizeInput('selectitems', 'Select Items Form 1', 
                   choices = it,
                   multiple = TRUE)
  })
  
  output$items_form2 <- renderUI({
    it_2 <- unique(params()[, input$idvar])
    selectizeInput('selectitems_2', 'Select Items Form 2', 
                   choices = it_2,
                   multiple = TRUE)
  })

  output$filtervars <- renderUI({
    vars <- names(params())
    selectizeInput('filter_var', "Filter Variables", choices = vars,
                   multiple = TRUE)
  })
  
  output$filter_2 <- renderUI({
    if(length(input$filter_var) > 0) {
      f_options <- lapply(1:length(input$filter_var), function(xx)
        unique(params()[, input$filter_var[xx]]))
      names(f_options) <- input$filter_var
      selectizeInput('filter_2_options', 'Filter Values:',
                     choices = f_options, multiple = TRUE)
    }
  })
  
  paramsA <- eventReactive(input$run, {
    mytext <- input$selectitems
    id_var <- input$idvar
    mytext <- ifelse(length(mytext) == 0, 
                     paste(1:nrow(params()), collapse = ","),
                     paste(mytext, collapse = ","))
    if(input$filter) {
      filt <- as.list(input$filter_var)
      filt_vals <- ifelse(input$filter_2_options == 'NA', 'NA', 
                          sQuote(input$filter_2_options))
      filt_vals <- paste(filt_vals, collapse = ",")
      filt <- paste(lapply(1:length(filt), function(xx) 
        paste0(filt[[xx]], ' %in% c(', filt_vals, ')')),
        collapse = input$filter_type)
      
      num_sel <- as.numeric(unlist(strsplit(mytext, ',\\s*')))
      subparams <- params() %>%
        filter_(paste(id_var, '%in% c(', mytext, ')')) %>%
        filter_(filt)
      return(subparams)
    } else {
      num_sel <- as.numeric(unlist(strsplit(mytext, ',\\s*')))
      subparams <- params() %>%
        filter_(paste(id_var, '%in% c(', mytext, ')'))
      return(subparams)
    }
  })
  
  paramsA_2 <- eventReactive(input$run2, {
    mytext_2 <- input$selectitems_2
    id_var <- input$idvar
    mytext_2 <- ifelse(length(mytext_2) == 0, 
                       paste(1:nrow(params()), collapse = ","),
                       paste(mytext_2, collapse = ","))
    if(input$filter) {
      filt <- as.list(input$filter_var)
      filt_vals <- ifelse(input$filter_2_options == 'NA', 'NA', 
                          sQuote(input$filter_2_options))
      filt_vals <- paste(filt_vals, collapse = ",")
      filt <- paste(lapply(1:length(filt), function(xx) 
        paste0(filt[[xx]], ' %in% c(', filt_vals, ')')),
        collapse = input$filter_type)
      
      num_sel <- as.numeric(unlist(strsplit(mytext_2, ',\\s*')))
      subparams <- params() %>%
        filter_(paste(id_var, '%in% c(', mytext_2, ')')) %>%
        filter_(filt)
      return(subparams)
    } else {
      num_sel <- as.numeric(unlist(strsplit(mytext_2, ',\\s*')))
      subparams <- params() %>%
        filter_(paste(id_var, '%in% c(', mytext_2, ')'))
      return(subparams)
    }
  })
  
  output$ip <- renderDataTable(paramsA())
  
  output$ip2 <- renderDataTable(paramsA_2())
  
  avgpars <- reactive({
    funcs <- sapply(1:length(input$param_vals), function(xx) 
      paste0('mean(', input$param_vals[xx], ')'))
    if(input$groups == FALSE) {
      avgpar <- paramsA() %>%
        select_(.dots = input$param_vals) %>%
        filter(complete.cases(.)) %>%
        summarise_(.dots = c(list("'Form 1'", "n()"), funcs)) %>%
        setNames(c('Form', 'N', unlist(funcs)))
    } else {
      avgpar <- paramsA() %>%
        select_(.dots = c(input$groupvar, input$param_vals)) %>%
        filter(complete.cases(.)) %>%
        group_by_(input$groupvar) %>%
        summarise_(.dots = c(list("'Form 1'", "n()"), funcs)) %>%
        setNames(c(input$groupvar, 'Form', 'N', unlist(funcs)))
    }
    
    if(input$compare == TRUE & input$groups == FALSE) {
      avgpar2 <- paramsA_2() %>%
        select_(.dots = input$param_vals) %>%
        filter(complete.cases(.)) %>%
        summarise_(.dots = c(list("'Form 2'", "n()"), funcs)) %>%
        setNames(c('Form', 'N', unlist(funcs)))
      avgpar <- rbind(avgpar, avgpar2)
    } else {
      if(input$compare == TRUE & input$groups == TRUE) {
        avgpar2 <- paramsA_2() %>%
          select_(.dots = c(input$groupvar, input$param_vals)) %>%
          filter(complete.cases(.)) %>%
          group_by_(input$groupvar) %>%
          summarise_(.dots = c(list("'Form 2'", "n()"), funcs)) %>%
          setNames(c(input$groupvar, 'Form', 'N', unlist(funcs)))
        avgpar <- rbind(avgpar, avgpar2)
      }
    }
    return(avgpar)
  })
  
  output$avgparams <- renderDataTable({
    avgpars()
  })
  
  output$Vars <- renderUI({
    vars <- names(paramsA())
    selectizeInput("groupvar", "Grouping Variables", choices = vars)
  })
  
  datasetInput <- reactive({
    if(length(input$dataset) == 1){
      switch(input$dataset,
             "Form 1" = paramsA(),
             "Form 2" = paramsA_2())
    } else {
      rbind(data.frame(paramsA(), form = "Form 1"),
            data.frame(paramsA_2(), form = "Form 2"))
    }
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(input$dataset, '.csv', sep='') 
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  iccdat <- reactive({
    if(input$groups == FALSE) {
      params2 <- paramsA() %>%
        select_(.dots = c(input$idvar, input$param_vals)) %>%
        filter(complete.cases(.))
      params2 <- data.frame(params2)
      
      t1 <- data.frame(drm(params2[, input$param_vals], seq(-5, 5, by = .05))@prob)
      item_names <- paste("item", params2[, input$idvar], sep = "_")
      colnames(t1) <- c("theta1", item_names)
      t1_names <- paste0(names(t1)[2], ':', names(t1)[ncol(t1)])
      t1 <- t1 %>%
        gather(item, prob, eval(parse(text = t1_names)))
    } else {
      params2 <- paramsA() %>%
        select_(.dots = c(input$idvar, input$groupvar, input$param_vals)) %>%
        filter(complete.cases(.))
      params2 <- data.frame(params2)
      
      t1 <- drm_groups(params2, input$groupvar, input$param_vals)
      uniq_groups <- sort(unique(params2[, input$groupvar]))
      item_names <- lapply(1:length(uniq_groups), function(xx)
        unique(paste("item", filter_(params2, paste0(input$groupvar, '==', 
                                          sQuote(uniq_groups[xx])))[, input$idvar], 
                     sep = "_")))
      # item_names <- unique(paste("item", paramsA()[, input$idvar], sep = "_"))
      t1 <- lapply(seq(t1), function(xx) {
        y <- data.frame(uniq_groups[xx], t1[[xx]])
        names(y) <- c(input$groupvar, "theta1", item_names[[xx]])
        t1_names <- paste0(item_names[[xx]][1], ':', 
               item_names[[xx]][length(item_names[[xx]])])
        y <- y %>%
          gather(item, prob, eval(parse(text = t1_names)))
        return(y)
      })
      t1 <- do.call("rbind", t1)
    }
    
  if(input$compare == TRUE & input$groups == FALSE) {
    params3 <- paramsA_2() %>%
      select_(.dots = c(input$idvar, input$param_vals)) %>%
      filter(complete.cases(.))
    params3 <- data.frame(params3)
    
    t2 <- data.frame(drm(params3[, input$param_vals], seq(-5, 5, by = .05))@prob)
    item_names <- paste("item", params3[, input$idvar], sep = "_")
    colnames(t2) <- c("theta1", item_names)
    t2_names <- paste0(names(t2)[2], ':', names(t2)[ncol(t2)])
    t2 <- t2 %>%
      gather(item, prob, eval(parse(text = t2_names)))
    
    t2$form <- 'Form 2'
    t1$form <- 'Form 1'
    t1 <- rbind(t1, t2)
  } else {
    if(input$compare == TRUE & input$groups == TRUE) {
      params3 <- paramsA_2() %>%
        select_(.dots = c(input$idvar, input$groupvar, input$param_vals)) %>%
        filter(complete.cases(.))
      params3 <- data.frame(params3)
      
      t2 <- drm_groups(params3, input$groupvar, input$param_vals)
      uniq_groups <- sort(unique(params3[, input$groupvar]))
      item_names <- lapply(1:length(uniq_groups), function(xx)
        unique(paste("item", filter_(params3, paste0(input$groupvar, '==', 
                                          sQuote(uniq_groups[xx])))[, input$idvar], 
                     sep = "_")))
      # item_names <- unique(paste("item", paramsA()[, input$idvar], sep = "_"))
      t2 <- lapply(seq(t2), function(xx) {
        y <- data.frame(uniq_groups[xx], t2[[xx]])
        names(y) <- c(input$groupvar, "theta1", item_names[[xx]])
        t2_names <- paste0(item_names[[xx]][1], ':', 
                           item_names[[xx]][length(item_names[[xx]])])
        y <- y %>%
          gather(item, prob, eval(parse(text = t2_names)))
        return(y)
      })
      t2 <- do.call("rbind", t2)
      
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
      f <- ggplot(iccdat(), aes(x = theta1, y = prob, color = factor(item))) + 
        theme_bw(base_size = 16)
      f <- f + geom_line(size = 1) + 
        geom_point(size = 0) + 
        scale_color_discrete("Item") + 
        scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                           breaks = seq(0, 1, by = .1)) + 
        scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
        theme(axis.title.y = element_text(vjust = 1.5), 
              axis.title.x = element_text(vjust = -0.25)) + 
        theme(panel.grid.major = element_line(colour = "#a7a7a7"))
    } else {
      if(input$compare == TRUE & input$groups == FALSE) {
        f <- ggplot(iccdat(), aes(x = theta1, y = prob, color = factor(item))) + 
          theme_bw(base_size = 16)
        f <- f + geom_line(size = 1) +
          geom_point(size = 0) + 
          scale_color_discrete("Item") + 
          scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                             breaks = seq(0, 1, by = .1)) + 
          scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
          theme(axis.title.y = element_text(vjust = 1.5), 
                axis.title.x = element_text(vjust = -0.25)) +
          theme(panel.grid.major = element_line(colour = "#a7a7a7")) +
          facet_grid(form ~ .)
      } else {
        if(input$groups == TRUE & input$compare == FALSE) {
          f <- ggplot(iccdat(), aes(x = theta1, y = prob, color = factor(item))) + 
            theme_bw(base_size = 16)
          f <- f + geom_line(size = 1) +
            geom_point(size = 0) + 
            scale_color_discrete("Item") + 
            scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                               breaks = seq(0, 1, by = .1)) + 
            scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
            theme(axis.title.y = element_text(vjust = 1.5), 
                  axis.title.x = element_text(vjust = -0.25)) + 
            theme(panel.grid.major = element_line(colour = "#a7a7a7")) +
            facet_grid(reformulate(input$groupvar, "."))
        } else {
          f <- ggplot(iccdat(), aes(x = theta1, y = prob, color = factor(item))) + 
            theme_bw(base_size = 16)
          f <- f + geom_line(size = 1) +
            geom_point(size = 0) + 
            scale_color_discrete("Item") + 
            scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                               breaks = seq(0, 1, by = .1)) + 
            scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
            theme(panel.grid.major = element_line(colour = "#a7a7a7")) +
            facet_grid(reformulate(input$groupvar, "form")) + 
            theme(axis.title.y = element_text(vjust = 1.5), 
                  axis.title.x = element_text(vjust = -0.25),
                  panel.margin = unit(0.35, "cm"))
        }
      }
    }
     f
  })
  
  output$click_info <- renderDataTable({
    dat <- iccdat()
    res <- nearPoints(iccdat(), input$plot1_click,
                      addDist = TRUE)
    item_number <- paste(gsub('item_', '', res$item), collapse = ",")
    
    if(input$compare == TRUE) {
      dat2 <- rbind(paramsA(), paramsA_2())
      tmp <- filter_(dat2, paste0(input$idvar, '%in% c(',
                                  item_number, ')'))
    } else {
      tmp <- filter_(paramsA(), paste0(input$idvar,  '%in% c(', 
                                       item_number, ')'))
    }
    
    datatable(tmp)
  })

  tccdat <- reactive({
    funcs <- sapply(1:length(input$param_vals), function(xx) 
      paste0('mean(', input$param_vals[xx], ')'))
    if(input$groups == FALSE) {
      params2_agg <- paramsA() %>%
        select_(.dots = input$param_vals) %>%
        filter(complete.cases(.)) %>%
        summarise_(.dots = funcs) %>%
        data.frame()
      t1_agg <- data.frame(drm(params2_agg, seq(-5, 5, by = .01))@prob)
    } else {
      params2_agg <- paramsA() %>%
        select_(.dots = c(input$groupvar, input$param_vals)) %>%
        filter(complete.cases(.)) %>%
        group_by_(input$groupvar) %>%
        summarise_(.dots = c(funcs)) %>%
        data.frame()
      t1_agg <- do.call("rbind", drm_groups(params2_agg, input$groupvar, 
                                            names(params2_agg)[2:ncol(params2_agg)]))
      t1_agg[, input$groupvar] <- rep(unique(params2_agg[, input$groupvar]), 
                                      each = 1001)
    }
    
    if(input$compare == TRUE & input$groups == FALSE) {
      params3_agg <- paramsA_2() %>%
        select_(.dots = input$param_vals) %>%
        filter(complete.cases(.)) %>%
        summarise_(.dots = funcs) %>%
        data.frame()
      t2_agg <- data.frame(drm(params3_agg, seq(-5, 5, by = .01))@prob)
      
      t2_agg$form <- 'Form 2'
      t1_agg$form <- 'Form 1'
      t1_agg <- rbind(t1_agg, t2_agg)
    } else {
      if(input$compare == TRUE & input$groups == TRUE) {
        params3_agg <- paramsA_2() %>%
          select_(.dots = c(input$groupvar, input$param_vals)) %>%
          filter(complete.cases(.)) %>%
          group_by_(input$groupvar) %>%
          summarise_(.dots = c(funcs)) %>%
          data.frame()
        t2_agg <- do.call("rbind", drm_groups(params3_agg, input$groupvar, 
                                              names(params3_agg)[2:ncol(params3_agg)]))
        t2_agg[, input$groupvar] <- rep(unique(params3_agg[, input$groupvar]), 
                                        each = 1001)
        t2_agg$form <- 'Form 2'
        t1_agg$form <- 'Form 1'
        t1_agg <- rbind(t1_agg, t2_agg)
      }
    }
    return(t1_agg)
  })
  
  output$tcc <- renderPlot({
    if(input$compare == FALSE & input$groups == FALSE) {
      f <- ggplot(iccdat(), aes(x = theta1, y = prob, color = factor(item))) + 
        theme_bw(base_size = 16)
      f<- f + geom_line(size = 1, alpha = .5, show_guide = FALSE) +
        #scale_linetype_discrete("Method") + 
        scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                           breaks = seq(0, 1, by = .1)) + 
        scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
        theme(axis.title.y = element_text(vjust = 1.5), 
              axis.title.x = element_text(vjust = -0.25)) + 
        geom_line(data = tccdat(), aes(x = theta1, y = item_1.1), size = 3, color = "black") +
        theme(panel.grid.major = element_line(colour = "#a7a7a7"))
    } else {
      if(input$compare == FALSE & input$groups == TRUE) {
        f <- ggplot(iccdat(), aes(x = theta1, y = prob, color = factor(item))) + 
          theme_bw(base_size = 16)
        f <- f + geom_line(size = 1) +
          scale_color_discrete("Item") + 
          scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                             breaks = seq(0, 1, by = .1)) + 
          scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
          theme(axis.title.y = element_text(vjust = 1.5), 
                axis.title.x = element_text(vjust = -0.25)) +
          geom_line(data = tccdat(), aes(x = theta1, y = item_1.1), size = 3, color = "black") +
          theme(panel.grid.major = element_line(colour = "#a7a7a7")) +
          facet_grid(reformulate(input$groupvar, "."))
      } else {
        if(input$compare == TRUE & input$groups == FALSE) {
          f <- ggplot(iccdat(), aes(x = theta1, y = prob, color = factor(item))) + 
            theme_bw(base_size = 16)
          f <- f + geom_line(size = 1, show_guide = FALSE) +
            #scale_linetype_discrete("Method") + 
            scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                               breaks = seq(0, 1, by = .1)) + 
            scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
            theme(axis.title.y = element_text(vjust = 1.5), 
                  axis.title.x = element_text(vjust = -0.25)) + 
            geom_line(data = tccdat(), aes(x = theta1, y = item_1.1), size = 3, color = "black")+ 
            theme(panel.grid.major = element_line(colour = "#a7a7a7")) +
            facet_grid(form ~ .)
        } else {
          f <- ggplot(iccdat(), aes(x = theta1, y = prob, color = factor(item))) + 
            theme_bw(base_size = 16)
          f <- f + geom_line(size = 1, show_guide = FALSE) +
            #scale_linetype_discrete("Method") + 
            scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                               breaks = seq(0, 1, by = .1)) + 
            scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
            theme(axis.title.y = element_text(vjust = 1.5), 
                  axis.title.x = element_text(vjust = -0.25)) + 
            geom_line(data = tccdat(), aes(x = theta1, y = item_1.1), size = 3, color = "black")+ 
            theme(panel.grid.major = element_line(colour = "#a7a7a7")) +
            facet_grid(reformulate(input$groupvar, "form"))
        }
      } 
    }
    f
  })
  
  output$tcc_comb <- renderPlot({
    if(input$compare == FALSE & input$groups == FALSE) {
      f <- ggplot(tccdat(), aes(x = theta1, y = item_1.1)) + 
        theme_bw(base_size = 16)
      f <- f + geom_line(size = 1, show_guide = FALSE) +
        geom_point(size = 0) + 
        scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                           breaks = seq(0, 1, by = .1)) + 
        scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
        theme(axis.title.y = element_text(vjust = 1.5), 
              axis.title.x = element_text(vjust = -0.25)) + 
        theme(panel.grid.major = element_line(colour = "#a7a7a7"))
    } else {
      if(input$compare == FALSE & input$groups == TRUE) {
        tccdat_local <- tccdat()
        tccdat_local[, input$groupvar] <- as.character(tccdat_local[, input$groupvar])
        f <- ggplot(tccdat_local, aes(x = theta1, y = item_1.1)) + 
          theme_bw(base_size = 16)
        f <- f + geom_line(size = 1, aes_string(color = input$groupvar, 
                                                group = input$groupvar)) +
          geom_point(size = 0, aes_string(color = input$groupvar)) +
          scale_color_discrete() + 
          scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                             breaks = seq(0, 1, by = .1)) + 
          scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
          theme(axis.title.y = element_text(vjust = 1.5), 
                axis.title.x = element_text(vjust = -0.25)) +
          theme(panel.grid.major = element_line(colour = "#a7a7a7"))
      } else {
        if(input$compare == TRUE & input$groups == FALSE) {
          f <- ggplot(tccdat(), aes(x = theta1, y = item_1.1, linetype = factor(form))) + 
            theme_bw(base_size = 16)
          f <- f + geom_line(size = 1) + 
            geom_point(size = 0) + 
            scale_linetype_discrete("Form") +
            scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                               breaks = seq(0, 1, by = .1)) + 
            scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
            theme(axis.title.y = element_text(vjust = 1.5), 
                  axis.title.x = element_text(vjust = -0.25)) + 
            theme(panel.grid.major = element_line(colour = "#a7a7a7"))
        } else {
          f <- ggplot(tccdat(), aes(x = theta1, y = item_1.1, linetype = factor(form))) + 
            theme_bw(base_size = 16)
          f <- f + geom_line(size = 1, aes_string(color = input$groupvar)) +
            geom_point(size = 0, aes_string(color = input$groupvar)) +
            scale_color_discrete("Item") + 
            scale_linetype_discrete("Form") +
            scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                               breaks = seq(0, 1, by = .1)) + 
            scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))+ 
            theme(axis.title.y = element_text(vjust = 1.5), 
                  axis.title.x = element_text(vjust = -0.25)) + 
            theme(panel.grid.major = element_line(colour = "#a7a7a7"))
        }
      } 
    }
    f
  })
  
  output$click_tcc_comb_info <- renderDataTable({
    dat <- tccdat()
    res <- nearPoints(tccdat(), input$click_tcc_comb, threshold = 10,
                      addDist = TRUE)
    grp <- paste(sQuote(unique(res[, input$groupvar])), collapse = ",")
    
    tmp <- avgpars() %>%
        filter_(.dots = paste0(input$groupvar, '%in% c(', 
                               grp, ")"))
    datatable(tmp)
  })
  
  output$tif <- renderPlot({
    if(input$groups == FALSE) {
      if(length(input$param_vals) == 1) {
        paramsA_sort <- paramsA() %>%
          select_(.dots = input$param_vals) %>%
          arrange_(input$param_vals[1]) %>%
          data.frame()
      } else {
        paramsA_sort <- paramsA() %>%
          select_(.dots = input$param_vals) %>%
          arrange_(input$param_vals[2]) %>%
          data.frame()
      }
      
      nitems <- nrow(paramsA_sort)
      item.inf <- item_inf(paramsA_sort, ability = seq(-5, 5, by = .01))
      # plots of individual items by grade - using cumsum across columns
      cinf <- do.call("c", lapply(1:nrow(item.inf), function(xx) cumsum(item.inf[xx, ])))
      item.cinf <- data.frame(ability = rep(seq(-5, 5, by = .01), each = ncol(item.inf)),
                              information = cinf)
      item.cinf$id <- rep(1:ncol(item.inf), times = 1001)
      item.cinf$group <- ifelse(item.cinf$id == nitems, 1, 0)
    } else {
      params_iif <- paramsA() %>%
        select_(.dots = c(input$groupvar, input$param_vals)) %>%
        filter(complete.cases(.)) %>%
        data.frame()
      
      item.inf <- iif_groups(params_iif, input$groupvar, 
                             names(params_iif)[2:ncol(params_iif)])
      cinf <- lapply(1:length(item.inf), function(xx) 
        do.call("c", lapply(1:nrow(item.inf[[xx]]), function(ii) 
          cumsum(item.inf[[xx]][ii, ]))))
      
      nitems <- sapply(item.inf, ncol)
      item.cinf <- do.call("rbind", lapply(1:length(item.inf), function(xx)
        data.frame(ability = rep(seq(-5, 5, by = .01), each = ncol(item.inf[[xx]])),
                   information = cinf[[xx]], 
                   id = rep(1:ncol(item.inf[[xx]]), times = 1001))))
      item.cinf[, input$groupvar] <- rep(as.matrix(unique(paramsA()[, input$groupvar])), 
                                         each = 1001*nitems[1])
      item.cinf$group <- ifelse(item.cinf$id == nitems[1], 1, 0)
    }
    
    if(input$compare == TRUE & input$groups == FALSE) {
      if(length(input$param_vals) == 1) {
        paramsA_sort_2 <- paramsA_2() %>%
          select_(.dots = input$param_vals) %>%
          arrange_(input$param_vals[1]) %>%
          data.frame()
      } else {
        paramsA_sort_2 <- paramsA_2() %>%
          select_(.dots = input$param_vals) %>%
          arrange_(input$param_vals[2]) %>%
          data.frame()
      }
      
      nitems <- nrow(paramsA_sort_2)
      item.inf_2 <- item_inf(paramsA_sort_2, ability = seq(-5, 5, by = .01))
      # plots of individual items by grade - using cumsum across columns
      cinf_2 <- do.call("c", lapply(1:nrow(item.inf_2), function(xx) cumsum(item.inf_2[xx, ])))
      item.cinf_2 <- data.frame(ability = rep(seq(-5, 5, by = .01), each = ncol(item.inf_2)),
                                information = cinf_2)
      item.cinf_2$id <- rep(1:ncol(item.inf_2), times = 1001)
      item.cinf_2$group <- ifelse(item.cinf_2$id == nitems, 1, 0)
      item.cinf_2$form <- 'Form 2'
      item.cinf$form <-'Form 1'
      item.cinf <- rbind(item.cinf, item.cinf_2)
    } else {
      if(input$compare == TRUE & input$groups == TRUE) {
        params_iif_2 <- paramsA_2() %>%
          select_(.dots = c(input$groupvar, input$param_vals)) %>%
          filter(complete.cases(.)) %>%
          data.frame()
        
        item.inf_2 <- iif_groups(params_iif_2, input$groupvar, 
                                 names(params_iif_2)[2:ncol(params_iif_2)])
        cinf_2 <- lapply(1:length(item.inf_2), function(xx) 
          do.call("c", lapply(1:nrow(item.inf_2[[xx]]), function(ii) 
            cumsum(item.inf_2[[xx]][ii, ]))))
        
        nitems_2 <- sapply(item.inf_2, ncol)
        item.cinf_2 <- do.call("rbind", lapply(1:length(item.inf_2), function(xx)
          data.frame(ability = rep(seq(-5, 5, by = .01), each = ncol(item.inf_2[[xx]])),
                     information = cinf_2[[xx]], 
                     id = rep(1:ncol(item.inf_2[[xx]]), times = 1001))))
        item.cinf_2[, input$groupvar] <- rep(as.matrix(unique(paramsA_2()[, input$groupvar])), 
                                           each = 1001*nitems_2[1])
        item.cinf_2$group <- ifelse(item.cinf_2$id == nitems_2[1], 1, 0)
        item.cinf_2$form <- 'Form 2'
        item.cinf$form <-'Form 1'
        item.cinf <- rbind(item.cinf, item.cinf_2)
      }
    }
    
    if(input$compare == FALSE & input$groups == FALSE) {
      f <- ggplot(item.cinf, aes(x = ability, y = information)) + theme_bw(base_size = 16)
      f <- f + geom_line(aes(group = id), color = "gray15", linetype = 2) + 
        scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1)) + 
        scale_y_continuous("Information")+ 
        geom_line(data = subset(item.cinf, group == 1), aes(x = ability, y = information), 
                  size = 1, linetype = 1, color = "black")
    } else {
      if(input$compare == FALSE & input$groups == TRUE) {
        f <- ggplot(item.cinf, aes(x = ability, y = information)) + theme_bw(base_size = 16)
        f <- f + geom_line(aes(group = id), color = "gray15", linetype = 2) + 
          scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1)) + 
          scale_y_continuous("Information")+ 
          geom_line(data = subset(item.cinf, group == 1), aes(x = ability, y = information), 
                    size = 1, linetype = 1, color = "black") + 
          facet_grid(reformulate(input$groupvar, "."))
      } else {
        if(input$compare == TRUE & input$groups == FALSE) {
          f <- ggplot(item.cinf, aes(x = ability, y = information)) + theme_bw(base_size = 16)
          f <- f + geom_line(aes(group = id), color = "gray15", linetype = 2) + 
            scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1)) + 
            scale_y_continuous("Information")+ 
            geom_line(data = subset(item.cinf, group == 1), aes(x = ability, y = information), size = 1, linetype = 1, color = "black") + 
            facet_grid(form ~ .)
        } else {
          if(input$compare == TRUE & input$groups == TRUE) {
            f <- ggplot(item.cinf, aes(x = ability, y = information)) + theme_bw(base_size = 16)
            f <- f + geom_line(aes(group = id), color = "gray15", linetype = 2) + 
              scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1)) + 
              scale_y_continuous("Information")+ 
              geom_line(data = subset(item.cinf, group == 1), aes(x = ability, y = information), 
                        size = 1, linetype = 1, color = "black") + 
              facet_grid(reformulate(input$groupvar, "form"))
          }
        }
      }
    }
    f
  })
  
})
