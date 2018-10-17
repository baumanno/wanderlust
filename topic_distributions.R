# Create topic distributions for users and alters.

# 0: Libraries ------------------------------------------------------------

library(tidyverse)
library(glue)
library(lubridate)

# 1: external function definitions ----------------------------------------

source("R/reading_data/read_snapshots.R")

# 2: Module constants ------------------------------------------------------

stroke_color <- "black"
stroke_size  <- .1
fill_alpha   <- .6
fill_palette <- "Blues"

# "formido" "cavedave" "IronWolve"
user <- "monocasa"

# 3: read data from snapshot files ----------------------------------------

ego_topics <- read_ego_topics(user)
alters_topics <- read_alters_topics(user)

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
    -author,
    -subreddit
  ) %>%
  group_by(year, month, topic) %>%
  summarise(num_posts = sum(count)) %>%
  mutate(prop = num_posts / sum(num_posts)) %>%
  ungroup()

# Plot topic distributions
ego_proportions %>%
  ggplot(mapping = aes(make_date(year, month), prop, fill = topic)) +
  geom_area(
    size = stroke_size,
    colour = stroke_color,
    alpha = fill_alpha,
    position = "stack"
  ) +
  labs(title = "Distribution of the ego's posts across topics",
       x = "",
       y = "# posts, relative",
       fill = "Topic") # +
# facet_wrap(. ~ year, scales = "free")

# Plot the cumsum of the absolute number of posts
ego_proportions %>%
  group_by(topic) %>%
  mutate(cs = cumsum(num_posts)) %>%
  ggplot(mapping = aes(make_date(year, month), cs)) +
  geom_line(mapping = aes(colour = topic)) +
  labs(x = "", y = "cum.sum of #posts", colour = "Topic")

# Plot the cumsum of the absolute number of posts for the user's first two years
# of activity to see early developments.
ego_proportions %>%
  filter(year < 2010) %>%
  group_by(topic) %>%
  mutate(cs = cumsum(num_posts)) %>%
  ggplot(mapping = aes(make_date(year, month), cs)) +
  geom_line(mapping = aes(colour = topic)) +
  labs(x = "", y = "cum.sum of #posts", colour = "Topic")

# 5: transform and plot alters data ---------------------------------------

# Compute proportions of topics that the alters are active in.
# See above.
alters_proportions <- alters_topics %>%
  unnest() %>%
  group_by(year, month, topic) %>%
  summarise(num_users = n()) %>%
  spread(key = topic,
         value = num_users,
         fill = 0) %>%
  gather(key = topic,
         value = num_users,
         factor_key = TRUE,
         -year,
         -month) %>%
  mutate(prop = num_users / sum(num_users)) %>%
  ungroup()

# Plot the distribution of alters by the topic they are most active in.
alters_proportions %>%
  ggplot(mapping = aes(make_date(year, month), prop, fill = topic)) +
  geom_area(
    size = stroke_size,
    colour = stroke_color,
    alpha = fill_alpha,
    position = "stack"
  ) +
  labs(title = "Distribution of the alters' posts across topics",
       x = "",
       y = "# posts, relative",
       fill = "Topic") #+  facet_wrap(. ~ year, scales = "free")

# Plot the cumsum of the absolute number of users
alters_proportions %>%
  group_by(topic) %>%
  mutate(cs = cumsum(num_users)) %>%
  ggplot(mapping = aes(make_date(year, month), cs)) +
  geom_line(mapping = aes(colour = topic)) +
  labs(x = "", y = "cum.sum of #users", colour = "Topic")

# Plot the cumsum of the absolute number of users for the user's first two years
# of activity to see early developments.
alters_proportions %>%
  filter(year < 2010) %>%
  group_by(topic) %>%
  mutate(cs = cumsum(num_users)) %>%
  ggplot(mapping = aes(make_date(year, month), cs)) +
  geom_line(mapping = aes(colour = topic)) +
  labs(x = "", y = "cum.sum of #users", colour = "Topic")

# 6: investigate cumsums --------------------------------------------------

# Filter out one topic, and plot the two cumsums
plot_topical_cumsums <- function(ego, alters, t) {
  a <- alters %>%
    filter(topic == t) %>%
    mutate(cs = cumsum(prop))
  e <- ego %>%
    filter(topic == t) %>%
    mutate(cs = cumsum(prop))

  ggplot() +
    geom_line(data = a, aes(make_date(year, month), cs, colour = "Alters")) +
    geom_line(data = e, aes(make_date(year, month), cs, colour = "Ego")) +
    labs(
      x = "",
      y = "proportion",
      colour = "Data",
      title = glue("Topic {t}")
    )
}

plot_topical_cumsums(ego_proportions, alters_proportions, 239)
plot_topical_cumsums(ego_proportions, alters_proportions, 235)
plot_topical_cumsums(ego_proportions, alters_proportions, 69)

# 7: analyze corr. of distributions ---------------------------------------

# join ego and alter data into one dataframe
corr_df <-
  left_join(ego_proportions,
            alters_proportions,
            by = c("month", "year", "topic")) %>%
  rename(prop_ego = prop.x, prop_alters = prop.y)

save(corr_df, file = glue("output/{user}_corr-df.rda"))

# filter out 0-0 rows; these occur a lot due to the spread-gather operations
corr_df <- corr_df %>%
  filter(prop_alters != 0 & prop_ego != 0 & topic != "<NA>")

# plot the proportions to identify possible correlations
corr_df %>%
  ggplot(mapping = aes(prop_ego, prop_alters, colour = topic)) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm") +
  labs(
    x = "prop. of posts",
    y = "prop. of users",
    title = user,
    colour = "Topic"
  ) +
  facet_wrap(. ~ topic)
