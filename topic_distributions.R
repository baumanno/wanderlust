# Create topic distributions for users and alters.

library(tidyverse)
library(glue)
library(lubridate)

source("R/reading_data/read_snapshot_file.R")

# 0: Module constants ------------------------------------------------------
stroke_color <- "black"
stroke_size  <- .1
fill_alpha   <- .6
fill_palette <- "Blues"

# 1: setup global state -----------------------------------------------------
user <- "monocasa" # , "formido", "cavedave", "IronWolve"
data_dir <- glue("data/{user}")

ego_topics <- read_monthly_snapshot_data(user, path = "{data_dir}/ego-topics-{year}-{month}.csv")
alters_topics <- read_monthly_snapshot_data(user, path = "{data_dir}/alters-topics-{year}-{month}.csv")

ego_topics_consolidated <- do.call(rbind, ego_topics)
alters_topics_consolidated <- do.call(rbind, alters_topics)

# Compute proportions of a topic an ego is active in.
# This can include 0-values if a user starts participating in a topic
# e.g. in 2011, but has not been active prior to that date.
ego_agg <- ego_topics_consolidated %>%
  spread(key = topic, value = count, fill = 0) %>%
  gather(key = topic,
         value = count,
         -year,
         -month,
         -author,
         -subreddit) %>%
  group_by(year, month, topic) %>%
  summarise(num_posts = sum(count)) %>%
  mutate(topics_rel = num_posts / sum(num_posts))

# Compute proportions of topics that the alters are active in.
# See above.
alters_agg <- alters_topics_consolidated %>%
  group_by(year, month, topic) %>%
  summarise(num_users = n()) %>%
  spread(key = topic, value = num_users, fill = 0) %>%
  gather(key = topic, value = num_users,-year,-month) %>%
  mutate(users_rel = num_users / sum(num_users))

# Plot topic distributions and facet by topic.
ego_agg %>%
  ggplot(mapping = aes(make_date(year, month), topics_rel, fill = factor(topic))) +
  geom_area(
    size = stroke_size,
    colour = stroke_color,
    alpha = fill_alpha,
    position = "stack"
  ) +
  labs(title = "Distribution of the ego's posts across topics",
       x = "",
       y = "# posts, relative") #+ facet_wrap(. ~ year, scales = "free")

alters_agg %>%
  ggplot(mapping = aes(make_date(year, month), users_rel, fill = factor(topic))) +
  geom_area(
    size = stroke_size,
    colour = stroke_color,
    alpha = fill_alpha,
    position = "stack"
  ) +
  labs(title = "Distribution of the alters' posts across topics",
       x = "",
       y = "# posts, relative") #+  facet_wrap(. ~ year, scales = "free")

# join ego and alter data into one dataframe
aligned_data <- ego_agg %>%
  left_join(alters_agg, by = c("year", "month", "topic"))

save(aligned_data, file = "output/aligned_data.rda")

# plot the proportions to identify possible correlations
ggplot(aligned_data,
       mapping = aes(users_rel, topics_rel, colour = factor(topic))) +
  geom_point(alpha = .15) +
  geom_abline() +
  facet_wrap(. ~ topic)
