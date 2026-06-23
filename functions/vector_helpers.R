v_calculate_components <- function(theta_degrees, v_length) {
  v_x <- v_length * sin(theta_degrees * pi / 180)
  v_y <- v_length * cos(theta_degrees * pi / 180)
  
  data.frame(v_x = v_x, v_y = v_y)
}


v_plot_components <- function(
    dat = NULL, 
    x = NULL, 
    y = NULL,
    labels = NULL,
    pal = NULL,
    lims = 1,
    legend_pos = c(0.2, 0.85),
    add_coast = TRUE,
    alpha_degree = 60 # coastline angle
) {
  
  
  if(is.null(x)) x <- dat[,1]
  if(is.null(y)) y <- dat[,2]
  if(is.null(labels)) labels <- dat[,3]
  
  #browser()
  
  if(length(x) < 3) {
    n_vec <- 3
  } else n_vec <- length(x)
  
  
  if(is.null(pal)) pal <- brewer.pal(n_vec, "Dark2")
  
  p_v <- ggplot() +
    geom_hline(yintercept = 0, col = "grey60") +
    geom_vline(xintercept = 0, col = "grey60") +
    scale_x_continuous("x", limits = c(-lims, lims)) +
    scale_y_continuous("y", limits = c(-lims, lims)) +
    coord_fixed(ratio = 1)
  
  if(isTRUE(add_coast)) {
    coast <- v_calculate_components(alpha_degree, 1.15*lims)
    
    p_v <- p_v +
      geom_segment(
        aes(x = -coast[,1], y = -coast[,2], xend = coast[,1], yend = coast[,2]), 
        col = "#e2d7b0", linewidth = 1.2
      ) 
  }
  
  if(!is.null(x)) {
    p_v <- p_v +
      geom_segment(
        aes(x = 0, y = 0, xend = x, yend = y, color = labels),
        arrow = arrow(length = unit(0.3, "cm"), type = "closed")
      ) +
      scale_colour_manual(values = pal) +
      theme(
        legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = legend_pos
      )
  }
  p_v
}




