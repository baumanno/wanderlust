# 0: Libraries ------------------------------------------------------------

library(tidyverse)
library(glue)
library(lubridate)

# 1: external function definitions ----------------------------------------

source("R/reading_data/read_snapshots.R")

# 2: Module constants ------------------------------------------------------

source("R/constants.R")
MIN_CS <- 5L
CS_THRESH <- 10L

# 3: read data from snapshot files ----------------------------------------

ego_topics <- read_ego_topics(USERNAME)
alters_topics <- read_alters_topics(USERNAME)

# 4: Load cached data -----------------------------------------------------

load(glue("output/{USERNAME}_corr-df.rda"))

# 5: Compute cumulative sums ----------------------------------------------

df <- corr_df %>%
  group_by(topic) %>%
  mutate(cs_ego = cumsum(prop_ego),
         cs_alters = cumsum(prop_alters))

# 6: Cumulative sums of indivdual proportions -----------------------------

# Plot the cumsum of the absolute number of posts
df %>%
  filter(max(cs_ego) >= MIN_CS) %>%
  ggplot(mapping = aes(date, cs_ego)) +
  geom_line(mapping = aes(colour = topic)) +
  labs(x = "", y = "cum.sum of #posts", colour = "Topic")

# Plot the cumsum of the absolute number of posts for the user's first two years
# of activity to see early developments.
df %>%
  filter(year < 2010) %>%
  ggplot(mapping = aes(date, cs_ego)) +
  geom_line(mapping = aes(colour = topic)) +
  labs(x = "", y = "cum.sum of #posts", colour = "Topic")

# Plot the cumsum of the proportion of alters in a topic
df %>%
  filter(max(cs_alters) >= MIN_CS) %>%
  ggplot(mapping = aes(date, cs_alters)) +
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
df %>%
  filter(year < 2010) %>%
  ggplot(mapping = aes(date, cs_alters)) +
  geom_line(mapping = aes(colour = topic)) +
  labs(x = "", y = "cum.sum of #users", colour = "Topic")


# 7: contrast cumsums of both proportions ---------------------------------

df %>%
  filter(max(cs_alters) >= CS_THRESH & max(cs_ego) >= CS_THRESH) %>%
  ggplot(mapping = aes(x = date)) +
  geom_line(mapping = aes(y = cs_alters, colour = "Alters")) +
  geom_line(mapping = aes(y = cs_ego, colour = "Ego")) +
  labs(x = "",
       y = "proportion",
       colour = "Data") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(
          angle = 65,
          vjust = 1,
          hjust = 1
        )) +
  facet_wrap( ~ topic)
