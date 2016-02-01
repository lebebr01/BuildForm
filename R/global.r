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

icc_plot <- function(data) {
  ggplot(data, aes(x = theta1, y = prob, color = factor(item))) + 
        theme_bw(base_size = 16) +
        geom_line(size = 1) + 
        geom_point(size = 0) + 
        scale_color_discrete("Item") + 
        scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                           breaks = seq(0, 1, by = .1)) + 
        scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1)) + 
        theme(axis.title.y = element_text(vjust = 1.5), 
              axis.title.x = element_text(vjust = -0.25)) + 
        theme(panel.grid.major = element_line(colour = "#a7a7a7"))
}

tcc_plot <- function(data, group = NULL, linetype = NULL) {
   ggplot(data, aes(x = theta1, y = item_1.1)) + 
        theme_bw(base_size = 16) + 
        geom_line(size = 1, show.legend = FALSE) +
        geom_point(size = 0) + 
        scale_y_continuous("Probability", limits = c(0, 1), expand = c(0, 0), 
                           breaks = seq(0, 1, by = .1)) + 
        scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1)) + 
        scale_color_discrete() + 
        theme(axis.title.y = element_text(vjust = 1.5), 
              axis.title.x = element_text(vjust = -0.25)) + 
        theme(panel.grid.major = element_line(colour = "#a7a7a7"))
}

tif_plot <- function(data) {
    ggplot(item.cinf, aes(x = ability, y = information)) + theme_bw(base_size = 16) +
        geom_line(aes(group = id), color = "gray15", linetype = 2) + 
        scale_x_continuous("Ability", limits = c(-5, 5), breaks = seq(-5, 5, by = 1)) + 
        scale_y_continuous("Information")+ 
        geom_line(data = subset(item.cinf, group == 1), aes(x = ability, y = information), 
                  size = 1, linetype = 1, color = "black")
}
