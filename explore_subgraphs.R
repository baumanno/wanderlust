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

# 4: load external objects ------------------------------------------------

load(glue("/home/oliver/Masterarbeit/output/{USERNAME}-graph_df.rda"))
load(glue("/home/oliver/Masterarbeit/output/{USERNAME}_corr-df.rda"))
load(glue("/home/oliver/Masterarbeit/output/{USERNAME}-graph_analysis.rda"))

# 5: compute measures on the graph data -----------------------------------

# all snapshots for one ego
ego_topics <- read_ego_topics(USERNAME)

# join graphs and topic data
df <-
  left_join(graph_data, ego_topics, by = c("year", "month", "author")) %>%
  rename(ego_topics = data) %>%
  mutate(
    date = make_date(year, month),
    topic_subgraph = map2(graph, ego_topics, filter_graph_wrapper)
  ) %>%
  drop_na()

# 6: plot data ------------------------------------------------------------

# The "topic-subgraph" contains all vertices that share a topic with the ego.
# For the edges in this subgraph, we can compute the ratio to the total number
# of edges, effectively asking: How big is the overlap of the topical subgraph
# and the full graph?
df %>%
  mutate(topical_overlap = map2_dbl(topic_subgraph, graph, function(a, b) {
    # compute ratio of edges of two graphs
    
    if (ecount(a) == 0 | ecount(b) == 0) {
      return(NA)
    }
    
    ecount(a) / ecount(b)
  })) %>%
  ggplot(mapping = aes(date, topical_overlap)) +
  geom_point(alpha = P_ALPHA) +
  geom_smooth() +
  labs(x = "",
       y = "ratio",
       title = "Overlap of the topic-subgraph and the full graph over time")

df %>%
  mutate(ratio_edges = map_dbl(topic_subgraph, directed_edge_ratio, user = USERNAME)) %>%
  ggplot(mapping = aes(date, ratio_edges)) +
  geom_point(alpha = P_ALPHA) +
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "",
       y = "topical_edges",
       title = "Relative to the entire graph, are topical edges mutual?")
