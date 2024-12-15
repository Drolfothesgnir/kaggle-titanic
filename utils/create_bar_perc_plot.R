create_bar_perc_plot <- function(data, xcol, fill_col, title = NULL, xlab = xcol, ylab = "Percentage") {
  title <- if (!is.null(title)) {
    title
  } else {
    sprintf("Fraction of %s grouped by %s", fill_col, xcol)
  }
  ggplot(data, aes(.data[[xcol]], fill = .data[[fill_col]])) +
    geom_bar(position = "fill") +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = title, x = xlab, y = ylab)
}