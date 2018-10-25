# Create topic distributions for users and alters.

# 0: Libraries ------------------------------------------------------------

library(tidyverse)
library(glue)
library(lubridate)

# 1: external function definitions ----------------------------------------

source("R/reading_data/read_snapshots.R")

# 2: Module constants ------------------------------------------------------

source("R/constants.R")

# 3: read data from snapshot files ----------------------------------------

ego_topics <- read_ego_topics(USERNAME)
alters_topics <- read_alters_topics(USERNAME)

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
    size = S_SIZE,
    colour = S_COLOR,
    alpha = F_ALPHA,
    position = "stack"
  ) +
  labs(title = "Distribution of the ego's posts across topics",
       x = "",
       y = "prop. posts",
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
  mutate(
    date = make_date(year, month),
    prop = num_users / sum(num_users)
    ) %>%
  ungroup()

# Plot the distribution of alters by the topic they are most active in.
alters_proportions %>%
  ggplot(mapping = aes(make_date(year, month), prop, fill = topic)) +
  geom_area(
    size = S_SIZE,
    colour = S_COLOR,
    alpha = F_ALPHA,
    position = "stack"
  ) +
  labs(title = "Distribution of alters, according to main topic of interest",
       subtitle = "For each snapshot-graph, we consider the topic that the alter was most active in",
       x = "",
       y = "prop. alters",
       fill = "Topic") #+  facet_wrap(. ~ year, scales = "free")

# Plot the cumsum of the absolute number of users
MIN_CS <- 350
alters_proportions %>%
  group_by(topic) %>%
  mutate(cs = cumsum(num_users)) %>%
  filter(max(cs) >= MIN_CS) %>%
  ggplot(mapping = aes(date, cs)) +
  geom_line(mapping = aes(colour = topic)) +
  geom_hline(yintercept = MIN_CS) +
  labs(
    x = "",
    y = "cum.sum of #users",
    colour = "Topic",
    title = "Cumulative sum of users",
    caption = glue("Black line denotes min. cumsum ({MIN_CS})")
  )

# Plot the cumsum of the absolute number of users for the user's first two years
# of activity to see early developments.
alters_proportions %>%
  filter(year < 2010) %>%
  group_by(topic) %>%
  mutate(cs = cumsum(num_users)) %>%
  ggplot(mapping = aes(date, cs)) +
  geom_line(mapping = aes(colour = topic)) +
  labs(x = "", y = "cum.sum of #users", colour = "Topic")

# 6: analyze corr. of distributions ---------------------------------------

# join ego and alter data into one dataframe
corr_df <-
  left_join(ego_proportions,
            alters_proportions,
            by = c("month", "year", "topic")) %>%
  rename(prop_ego = prop.x, prop_alters = prop.y)

# filter out 0-0 rows; these occur a lot due to the spread-gather operations
corr_df <- corr_df %>%
  filter(prop_alters != 0 & prop_ego != 0 & topic != "<NA>")

# plot the proportions to identify possible correlations
corr_df %>%
  ggplot(mapping = aes(prop_ego, prop_alters, colour = topic)) +
  geom_point(alpha = P_ALPHA) +
  geom_smooth(method = "lm") +
  labs(
    x = "prop. of posts",
    y = "prop. of users",
    title = USERNAME,
    colour = "Topic"
  ) +
  facet_wrap(. ~ topic)

# 7: investigate cumsums --------------------------------------------------

# Filter out one topic, and plot the two cumsums
plot_topical_cumsums <- function(df, cs_thresh = 10) {
  df %>%
    group_by(topic) %>%
    mutate(cs_alters = cumsum(prop_alters),
           cs_ego = cumsum(prop_ego)) %>%
    filter(max(cs_alters) >= cs_thresh & max(cs_ego) >= cs_thresh) %>%
    ggplot(mapping = aes(x = date)) +
    geom_line(mapping = aes(y = cs_alters, colour = "Alters")) +
    geom_line(mapping = aes(y = cs_ego, colour = "Ego")) +
    labs(x = "",
         y = "proportion",
         colour = "Data") +
    facet_wrap(~ topic)
}

plot_topical_cumsums(corr_df, cs_thresh = 10L)

save(corr_df, file = glue("output/{USERNAME}_corr-df.rda"))
