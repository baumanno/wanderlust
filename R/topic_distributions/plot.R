library(tidyverse)

source("R/constants.R")

area_chart <- function(df) {
  df %>%
    ggplot(mapping = aes(date, prop, fill = topic)) +
    geom_area(
      size = S_SIZE,
      colour = S_COLOR,
      alpha = F_ALPHA,
      position = "stack"
    ) +
    labs(x = "date",
         y = "proportion")  +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      date_minor_breaks = "1 month"
    )
}

fill_colours <- function(topics) {
  n_topics <- length(topics)
  
  # Brewer-palette suitable for qualitative data
  pal <- brewer.pal(8, "Dark2")
  
  # inflate palette by interpolating to `num_topics` colors
  cols <- colorRampPalette(pal)(n_topics)
  
  # must be named list to index properly
  names(cols) <- topics
  
  cols
}