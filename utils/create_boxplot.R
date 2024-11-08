library(tidyverse)
library(ggthemes)

create_boxplot <- function(data,
                           x_col,
                           y_col,
                           title = NULL,
                           x_label = NULL,
                           y_label = NULL,
                           fill_col = NULL,
                           add_mean_points = FALSE,
                           dodge = FALSE,
                           rotate_labels = FALSE,
                           box_color = "#69b3a2"
                           ) {
  # Create base plot
  p <- ggplot(data, aes(.data[[x_col]], .data[[y_col]])) +
    theme_minimal() +
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = if (rotate_labels)
        45
        else
          0, hjust = if (rotate_labels)
            1
        else
          0.5),
      plot.title = element_text(size = 14, face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  # Add fill if specified
  if (!is.null(fill_col)) {
    p <- p + aes(fill = .data[[fill_col]])
    if (dodge) {
      p <- p + geom_boxplot(position = "dodge")
    } else {
      p <- p + geom_boxplot()
    }
  } else {
    p <- p + geom_boxplot(fill = box_color)
  }
  
  # Add labels
  p <- p +
    labs(
      title = title,
      x = if (is.null(x_label))
        x_col
      else
        x_label,
      y = if (is.null(y_label))
        y_col
      else
        y_label
    )
  
  # Add mean points
  if (add_mean_points == TRUE) {
    p <- p + stat_summary(
      fun = mean,
      geom = "point",
      shape = 23,
      size = 3,
      fill = "white"
    )
  }
  
  # Add number of observations
  p <- p + stat_summary(
    fun.data = function(x) {
      return(c(y = max(x) + 2, label = length(x)))
    },
    geom = "text",
    size = 3
  )
  
  return(p)
}