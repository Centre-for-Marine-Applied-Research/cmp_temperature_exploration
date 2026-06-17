v_calculate_components <- function(theta_degrees, v_length) {
  v_x <- v_length * sin(theta_degrees * pi / 180)
  v_y <- v_length * cos(theta_degrees * pi / 180)
  
  data.frame(v_x = v_x, v_y = v_y)
}


v_plot_components <- function(
    theta_degrees, 
    v_length = 1, 
    add_components = TRUE, 
    pal = NULL,
    lims = 1,
    legend_pos = c(0.8, 0.9),
    add_coast = FALSE,
    alpha_degree = 60 # coastline angle
) {
  
  v <- v_calculate_components(theta_degrees, v_length) %>% 
    mutate(
      theta_degrees = ordered(theta_degrees, levels = sort(theta_degrees))
    )
  
  if(nrow(v) < 3) {
    n_theta <- 3
  } else n_theta <- nrow(v)
  
  if(is.null(pal)) pal <- brewer.pal(n_theta, "Dark2")
  
  p_v <- ggplot() +
    geom_hline(yintercept = 0, col = "grey60") +
    geom_vline(xintercept = 0, col = "grey60") +
    scale_x_continuous("x", limits = c(-lims, lims)) +
    scale_y_continuous("y", limits = c(-lims, lims)) +
    coord_fixed(ratio = 1)
  
  
  if(isTRUE(add_coast)) {
    p_v <- p_v +
      v_add_coast_segment(alpha_degree, lims) 
  }
  
  p_v <- p_v +
    geom_segment(
      aes(x = 0, y = 0, xend = v[,1], yend = v[,2], col = v[,3]),
      arrow = arrow(length = unit(0.3, "cm"), type = "closed")
    ) +
    scale_colour_manual("theta (degrees)", values = pal) +
    theme(
      legend.position = "inside",
      legend.position.inside = legend_pos
    )
  
  if(isTRUE(add_components)) {
    
    p_v <- p_v +
      # x component
      geom_segment(
        aes(x = 0, xend = v[,1], y = v[,2], yend = v[,2]),
        arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
        linetype = 2,
      ) +
      # y component
      geom_segment(
        aes(x = 0, xend = 0, y = 0, yend = v[,2]),
        arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
        linetype = 2
      )
  }
  
  p_v
}



v_add_coast_segment <- function(alpha_degree, lims) {
  
  coast <- v_calculate_components(alpha_degree, 1.15*lims)
  
  geom_segment(
    aes(
      x = -coast[,1], y = -coast[,2], xend = coast[,1], yend = coast[,2]
    ), col = "#e2d7b0", linewidth = 1.2
  ) 
}




