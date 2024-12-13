create_pie_chart <- function(data, title, lab_fill, colors = NULL) {
  # Calculate the positions for labels
  data <- data %>%
    arrange(desc(status)) %>%  # Sort by status to ensure consistent ordering
    mutate(
      prop = count/sum(count),
      percentage = prop,
      ypos = cumsum(prop) - prop/2,
      label = scales::percent(prop, accuracy = 0.1)
    )
  
  p <- ggplot(data, aes(x = "", y = prop, fill = status)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(
      aes(y = ypos, label = label),
      color = "white",
      size = 6
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      plot.title = element_text(
        hjust = 0.5,
        size = 16,
        face = "bold"
      ),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    ) +
    labs(title = title, fill = lab_fill)
  
  if (!is.null(colors)) {
    p <- p + scale_fill_manual(values = colors)
  }
  
  return(p)
}