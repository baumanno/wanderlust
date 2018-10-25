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
  group_by(topic) %>% 
  filter(n() >= MIN_OBS) %>% 
  ggplot(mapping = aes(prop_ego, prop_alters, colour = topic)) +
  geom_point(alpha = P_ALPHA) +
  geom_abline() +
  geom_smooth(method = "lm") +
  labs(
    x = "prop. of posts",
    y = "prop. of users",
    title = USERNAME,
    colour = "Topic"
  ) +
  facet_wrap(. ~ topic)

corr_df %>% 
  group_by(topic) %>% 
  filter(n() >= MIN_OBS) %>% 
  mutate(
    pa_pe = prop_ego * prop_alters
  ) %>% 
  ggplot(mapping = aes(date, pa_pe, colour = topic)) +
  geom_point(alpha = P_ALPHA) +
  geom_abline() +
  geom_smooth() +
  facet_wrap(~topic)

save(corr_df, file = glue("output/{USERNAME}_corr-df.rda"))
