# Create topic distributions for users and alters.

# 0: Libraries ------------------------------------------------------------

library(tidyverse)
library(glue)
library(lubridate)
library(grid)
library(RColorBrewer)
library(parallel)

# 1: external function definitions ----------------------------------------

source("R/reading_data/read_snapshots.R")

# 2: Module constants ------------------------------------------------------

source("R/constants.R")
usernames <- c("monocasa", "cavedave", "formido", "IronWolve")
MIN_OBS <- 5

# parallel execution across all cores to plot data for multiple users
mclapply(usernames, mc.cores = 4L, function(user) {
  # 3: read data from snapshot files ----------------------------------------
  
  date_range <-
    seq.Date(as.Date("2007-01-01"),
             as.Date("2018-02-01"),
             by = "1 month")
  
  ego_topics <- read_ego_topics(user = user, date = date_range)
  alters_topics <- read_alters_topics(user = user, date = date_range)
  
  # 4: transform and plot ego data ------------------------------------------
  
  # Compute proportions of a topic an ego is active in.
  # This can include 0-values if a user starts participating in a topic
  # e.g. in 2011, but has not been active prior to that date.
  ego_proportions <- ego_topics %>%
    unnest() %>%
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
    group_by(date, topic) %>%
    summarise(num_posts = sum(count)) %>%
    mutate(prop = num_posts / sum(num_posts)) %>%
    ungroup()
  
  # Compute proportions of topics that the alters are active in.
  # See above.
  alters_proportions <- alters_topics %>%
    unnest() %>%
    group_by(date, topic) %>%
    summarise(num_users = n()) %>%
    filter(num_users > 1, !is.na(topic)) %>%
    spread(key = topic,
           value = num_users,
           fill = 0) %>%
    gather(key = topic,
           value = num_users,
           factor_key = TRUE,
           -date) %>%
    mutate(prop = num_users / sum(num_users)) %>%
    ungroup()
  
  # 5: plot topic distributions ---------------------------------------------
  
  # Make sure both plots (ego & alters) user same colorscale
  
  # figure out what topics are contained in the data
  topics_present <-
    union(ego_proportions$topic, alters_proportions$topic)
  num_topics <- length(topics_present)
  
  # Brewer-palette suitable for qualitative data
  p <- brewer.pal(8, "Dark2")
  
  # inflate palette by interpolating to `num_topics` colors
  TOPIC_COLORS <- colorRampPalette(p)(num_topics)
  
  # must be named list to index properly
  names(TOPIC_COLORS) <- topics_present
  
  # Plot topic distributions
  dist_ego <-
    ego_proportions %>%
    ggplot(mapping = aes(date, prop, fill = topic)) +
    geom_area(
      size = S_SIZE,
      colour = S_COLOR,
      alpha = F_ALPHA,
      position = "stack"
    ) +
    scale_fill_manual("Topic", values = TOPIC_COLORS) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      date_minor_breaks = "1 month"
    ) +
    labs(# title = "Distribution of the ego's posts across topics",
      x = "",
      y = "prop. posts")
  
  # Plot the distribution of alters by the topic they are most active in.
  dist_alters <-
    alters_proportions %>%
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
      date_breaks = "1 year",
      date_labels = "%Y",
      date_minor_breaks = "1 month"
    ) +
    scale_fill_manual(
      "Topic",
      breaks = topics_present,
      values = TOPIC_COLORS,
      guide = guide_legend(
        direction = "horizontal",
        title.position = "top",
        label.position = "bottom",
        nrow = 1
      )
    )
  
  # https://stackoverflow.com/questions/16367835/preserve-proportion-of-graphs-using-grid-arrange
  g <-
    rbind(ggplotGrob(dist_ego + theme(legend.position = "none")),
          ggplotGrob(dist_alters),
          size = "last")
  
  ggsave(plot = g,
         file = glue("figs/{user}_distributions.png"))
  
  # 6: analyze corr. of distributions ---------------------------------------
  
  # join ego and alter data into one dataframe
  corr_df <-
    left_join(ego_proportions,
              alters_proportions,
              by = c("date", "topic")) %>%
    rename(prop_ego = prop.x, prop_alters = prop.y)
  
  save(corr_df, file = glue("output/{USERNAME}_corr-df.rda"))
  
  # filter out 0-0 rows; these occur a lot due to the spread-gather operations
  corr_df <- corr_df %>%
    filter(prop_alters != 0 & prop_ego != 0 & topic != "<NA>")
  
  # plot the proportions to identify possible correlations
  p <- corr_df %>%
    group_by(topic) %>%
    filter(n() >= MIN_OBS) %>%
    ggplot(mapping = aes(prop_ego, prop_alters, colour = topic)) +
    geom_point(alpha = P_ALPHA) +
    geom_abline() +
    geom_smooth(method = "lm") +
    labs(
      x = "prop. of posts",
      y = "prop. of users",
      title = user,
      colour = "Topic"
    ) +
    facet_wrap(. ~ topic)
  
  ggsave(p, file = glue("figs/{user}_prop_ego_alter.png"))
  
  p <- corr_df %>%
    group_by(topic) %>%
    filter(n() >= MIN_OBS) %>%
    mutate(pa_pe = prop_ego * prop_alters) %>%
    ggplot(mapping = aes(date, pa_pe, colour = topic)) +
    geom_point(alpha = P_ALPHA) +
    geom_abline() +
    geom_smooth() +
    facet_wrap(~ topic)
  
  ggsave(p, file = glue("figs/{user}_interest.png"))
})
