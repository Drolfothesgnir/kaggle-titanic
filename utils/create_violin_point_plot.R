create_violin_point_plot <- function(data, x_col, y_col, 
                                     alpha_points = 0.4, 
                                     jitter_width = 0.2) {
  # Get counts for each group
  counts <- data %>%
    group_by(.data[[x_col]]) %>%
    summarise(n = n(), .groups = "drop")
  
  # Create the plot
  p <- ggplot(data, aes(x = factor(.data[[x_col]]), y = .data[[y_col]])) +
    # Add violin plot
    geom_violin(fill = "#69b3a2", alpha = 0.5) +
    # Add individual points
    geom_jitter(alpha = alpha_points, width = jitter_width, color = "#2c3e50") +
    # Add mean points
    stat_summary(fun = mean, geom = "point", size = 3, color = "red", shape = 23, fill = "red") +
    # Add labels
    labs(
      title = paste("Distribution of", y_col, "by", x_col),
      x = x_col,
      y = y_col
    ) +
    # Add sample size labels
    geom_text(data = counts, 
              aes(y = max(data[[y_col]]) + 2, 
                  label = paste("n =", n)),
              size = 3) +
    theme_minimal() +
    theme(
      text = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# Example usage:
# df %>% 
#   create_violin_point_plot("family_size")