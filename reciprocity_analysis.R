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

filter_graph_wrapper <- function(graph, ego) {
  filter_graph(graph, ego$topic)
}

ratio_edges <- function(subgraph, fullgraph) {
  if (vcount(fullgraph) == 0 || vcount(subgraph) == 0) {
    return(NA)
  }
  
  ecount(subgraph) / ecount(fullgraph)
}

# 3: module constants -----------------------------------------------------

source("R/constants.R")

# 4: load external objects ------------------------------------------------

load(glue("output/{USERNAME}-graph_df.rda"))
load(glue("output/{USERNAME}_corr-df.rda"))
load(glue("output/{USERNAME}-graph_analysis.rda"))

# 5: compute measures on the graph data -----------------------------------

# all snapshots for one ego
ego_topics <- read_ego_topics(USERNAME)

# join graphs and topic data
df <-
  left_join(graph_data, ego_topics, by = c("year", "month", "author")) %>%
  rename(ego_topics = data) %>%
  mutate(
    date = make_date(year, month),
    topical_subgraph = map2(graph, ego_topics, filter_graph_wrapper),
    kp_subgraph = map_dbl(topical_subgraph, katz_powell_mutuality)
  )

# The Katz-Powell index of mutuality indicates whether edge choices between nodes 
# are reciprocated. Computing this measure on the subgraph indicates whether edges
# tend to be mutual if ego and alters share interests.
df %>%
  ggplot(mapping = aes(date, kp_subgraph)) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = P_ALPHA) +
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(
    x = "",
    y = "kp (subgraph)",
    title = "Katz-Powell index of the topical subgraph over time",
    caption = "1: all choices are reciprocated,\n0: no tendency to reciprocate,\n< 0: too few mutual dyads observed"
  )

ggsave(filename = glue("figs/{USERNAME}-3-katz-powell.png"))

# We can measure reciprocity both as a ratio of incoming and outgoing edges, and
# by computing the Katz-Powell index of mutuality. This plot shows that they
# both arrive at similar conclusions.
df %>%
  mutate(rec = map_dbl(topical_subgraph, reciprocity, mode = "ratio")) %>%
  ggplot(mapping = aes(x = date)) +
  geom_hline(yintercept = 0) +
  geom_point(mapping = aes(y = rec, colour = "reciprocity ratio"),
             alpha = P_ALPHA) +
  geom_smooth(mapping = aes(y = rec, colour = "reciprocity ratio")) +
  geom_point(mapping = aes(y = kp_subgraph, colour = "Katz-Powell index"),
             alpha = P_ALPHA) +
  geom_smooth(mapping = aes(y = kp_subgraph, colour = "Katz-Powell index")) +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(
    x = "",
    y = "value",
    colour = "Measure",
    title = "Different reciprocity measures of the topical subgraph"
  )

ggsave(filename = glue("figs/{USERNAME}-5-rec-subgraph-1.png"))