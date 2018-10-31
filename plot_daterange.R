# 0: Libraries ------------------------------------------------------------

library(tidyverse)
library(glue)
library(lubridate)
library(grid)
library(RColorBrewer)

# 1: external function definitions ----------------------------------------

source("R/reading_data/read_snapshots.R")

# 2: Module constants ------------------------------------------------------

source("R/constants.R")

MIN_OBS <- 5


ego_prop <- function(user, lower, upper) {
  assertthat::assert_that(is.Date(lower),
                          is.Date(upper),
                          msg = "Lower and upper bound must be dates")
  
  egos <- read_ego_topics(user)
  
  ego_filtered <- egos %>%
    unnest() %>%
    mutate(date = make_date(year, month)) %>%
    filter(date >= lower & date <= upper) %>%
    spread(key = topic,
           value = count,
           fill = 0) %>%
    gather(
      key = topic,
      value = count,
      factor_key = TRUE,
      -year,
      -month,
      -date,
      -author,
      -subreddit
    ) %>%
    group_by(year, month, topic) %>%
    summarise(num_posts = sum(count)) %>%
    mutate(date = make_date(year, month),
           prop = num_posts / sum(num_posts)) %>%
    ungroup()
  
  save(ego_filtered, file = glue("output/{user}_ego-filtered.rda"))
  
  ego_filtered
}

alters_prop <- function(user, lower, upper) {
  assertthat::assert_that(is.Date(lower),
                          is.Date(upper),
                          msg = "Lower and upper bound must be dates")
  
  alters <- read_alters_topics(user)
  
  alters_filtered <- alters %>%
    unnest() %>%
    mutate(date = make_date(year, month)) %>%
    filter(date >= lower & date <= upper) %>%
    group_by(year, month, topic) %>%
    summarise(num_users = n()) %>%
    filter(num_users > 1) %>%
    filter(!is.na(topic)) %>%
    spread(key = topic,
           value = num_users,
           fill = 0) %>%
    gather(key = topic,
           value = num_users,
           factor_key = TRUE,
           -year,
           -month) %>%
    mutate(date = make_date(year, month),
           prop = num_users / sum(num_users)) %>%
    ungroup()
  
  save(alters_filtered,
       file = glue("output/{user}_alters-filtered.rda"))
  
  alters_filtered
}

topic_colors <- function(...) {
  topics_present <- union(...)
  num_topics <- length(topics_present)
  
  p <- brewer.pal(8, "Dark2")
  
  topic_colors <- colorRampPalette(p)(num_topics)
  names(topic_colors) <- topics_present
  
  topic_colors
}

plot_ego_props <- function(df, topic_colors) {
  df %>%
    ggplot(mapping = aes(date, prop, fill = topic)) +
    geom_area(
      size = S_SIZE,
      colour = S_COLOR,
      alpha = F_ALPHA,
      position = "stack"
    ) +
    scale_fill_manual("Topic", values = topic_colors) +
    scale_x_date(
      date_breaks = "3 months",
      date_labels = "%m/%y",
      date_minor_breaks = "1 month"
    ) +
    labs(# title = "Distribution of the ego's posts across topics",
      x = "",
      y = "prop. posts")
}

plot_alters_props <- function(df, topic_colors) {
  df %>%
    ggplot(mapping = aes(date, prop, fill = topic)) +
    geom_area(
      size = S_SIZE,
      colour = S_COLOR,
      alpha = F_ALPHA,
      position = "stack"
    ) +
    labs(# title = "Distribution of alters, according to main topic of interest",
      # subtitle = "For each snapshot-graph, we consider the topic that the alter was most active in",
      x = "",
      y = "prop. alters")  +
    theme(legend.position = "bottom") +
    scale_x_date(
      date_breaks = "3 months",
      date_labels = "%m/%y",
      date_minor_breaks = "1 month"
    ) +
    scale_fill_manual(
      "Topic",
      values = topic_colors,
      guide = guide_legend(
        direction = "horizontal",
        title.position = "top",
        label.position = "bottom",
        nrow = 1
      )
    )
}

plot_dists <- function(ego, alters, username = NULL) {
  t_cols <- topic_colors(ego$topic, alters$topic)
  p_e <- plot_ego_props(ego, t_cols)
  p_a <- plot_alters_props(alters, t_cols)
  
  combined <-
    rbind(ggplotGrob(p_e + theme(legend.position = "none")),
          ggplotGrob(p_a + labs(caption = username)),
          size = "last")
  
  if (!is.null(username)) {
    ggsave(glue("figs/{username}_daterange.png"))
  }
  
  grid.draw(combined)
}

# monocasa ----------------------------------------------------------------
m_e <-
  ego_prop("monocasa",
           lower = make_date(2013, 12),
           upper = make_date(2015, 1))
m_a <-
  alters_prop("monocasa",
              lower = make_date(2013, 12),
              upper = make_date(2015, 1))

plot_dists(m_e, m_a, "monocasa")

# formido -----------------------------------------------------------------

f_e <-
  ego_prop("formido",
           lower = make_date(2015, 1),
           upper = make_date(2017, 8))
f_a <-
  alters_prop("formido",
              lower = make_date(2015, 1),
              upper = make_date(2017, 8))

plot_dists(f_e, f_a, "formido")

# cavedave ----------------------------------------------------------------

c_e <-
  ego_prop("cavedave",
           lower = make_date(2010, 3),
           upper = make_date(2013, 6))
c_a <-
  alters_prop("cavedave",
              lower = make_date(2010, 3),
              upper = make_date(2013, 6))

plot_dists(c_e, c_a, "cavedave")
