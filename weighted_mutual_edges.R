# Explore correlations between topic distributions and properties
# of the user's social graph.

# 0: libraries ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(glue)

# 1: external function definitions ----------------------------------------

source("R/network_measures/katz_powell.R")
source("R/network_measures/weighted_mutuality.R")
source("R/network_construction/filter_graph.R")
source("R/reading_data/read_snapshots.R")

# 2: helper functions -----------------------------------------------------

# This wrapper accepts a dataframe of ego-topics and pulls out the topics.
filter_graph_wrapper <- function(graph, ego) {
  filter_graph(graph, ego$topic)
}

# 3: module constants -----------------------------------------------------

source("R/constants.R")
MIN_OBS <- 5

# 4: load external objects ------------------------------------------------

load(glue("/home/oliver/Masterarbeit/output/{USERNAME}-graph_df.rda"))
load(glue("/home/oliver/Masterarbeit/output/{USERNAME}_corr-df.rda"))
load(glue("/home/oliver/Masterarbeit/output/{USERNAME}-graph_analysis.rda"))

# 5: compute measures on the graph data -----------------------------------

# all snapshots for one ego
ego_topics <- read_ego_topics(USERNAME)

# construct all required data from graphs and proportions
df <-
  graph_data %>%
  left_join(ego_topics, by = c("year", "month", "author")) %>%
  rename(ego_topics = data) %>%
  drop_na() %>%
  left_join(corr_df, by = c("year", "month")) %>%
  mutate(
    date = make_date(year, month),
    topic_subgraph = map2(graph, ego_topics, filter_graph_wrapper),
    topical_overlap = map2_dbl(topic_subgraph, graph, function(a, b) {
      # compute ratio of edges of two graphs
      
      if (ecount(a) == 0 | ecount(b) == 0) {
        return(NA)
      }
      
      ecount(a) / ecount(b)
    }),
    ratio_edges = map_dbl(topic_subgraph, directed_edge_ratio, user = USERNAME),
    weighted_ratio = ratio_edges * prop_ego
  ) %>%
  drop_na() %>%
  select(date,
         topic,
         topical_overlap,
         ratio_edges,
         prop_ego,
         prop_alters,
         weighted_ratio)

# 6: plot data ------------------------------------------------------------

df %>%
  group_by(topic) %>%
  filter(n() > MIN_OBS) %>%
  ggplot(mapping = aes(prop_ego, topical_overlap, colour = topic)) +
  geom_point(alpha = P_ALPHA) +
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "prop. of Ego's posts",
    y = "topical edges in complete graph",
    title = "Proportion of topical edges in graph vs prop. of posts to a topic",
    caption = glue("Minimum number of observations: {MIN_OBS}")
  ) +
  facet_wrap( ~ topic)

ggsave(filename = glue("figs/{USERNAME}-WEIGHTS-prop-vs-overlap.png"))

df %>%
  group_by(topic) %>%
  filter(n() > MIN_OBS) %>%
  ggplot(mapping = aes(date, weighted_ratio, colour = topic)) +
  geom_point(alpha = P_ALPHA) +
  geom_smooth(method = "lm") +
  facet_wrap(~ topic) +
  labs(
    x = "",
    y = "weighted reciprocity",
    title = "Weight edge difference by proportion of posts",
    caption = glue("Minimum number of observations: {MIN_OBS}")
  )

# new
df %>%
  group_by(topic) %>%
  filter(n() > MIN_OBS) %>%
  ggplot(mapping = aes(prop_ego, weighted_ratio, colour = topic)) +
  geom_point(alpha = P_ALPHA) +
  geom_smooth(method = "lm") +
  geom_abline() +
  facet_wrap(~ topic) +
  labs(
    x = "prop ego",
    y = "weighted reciprocity",
    title = "Weight edge difference by proportion of posts",
    caption = glue("Minimum number of observations: {MIN_OBS}")
  )

ggsave(filename = glue(
  "figs/{USERNAME}-WEIGHTS-weighted-relative-reciprocity.png"
))

graphs_and_topics %>%
  mutate(cs = cumsum(topical_overlap)) %>%
  ggplot(mapping = aes(date, cs)) +
  geom_line() +
  labs(x = "",
       y = "cumsum",
       title = "Cumsum of ratio of topical edges")

ggsave(filename = glue("figs/{USERNAME}-WEIGHTS-cumsum-topical-overlap.png"))
