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
  df %>%
  mutate(
    date = make_date(year, month),
    topical_subgraph = map2(graph, ego_topics, filter_graph_wrapper),
    topical_edges = map2_dbl(topical_subgraph,
                             graph,
                             relative_reciprocity,
                             user = USERNAME),
    rat_topical_edges = map2_dbl(topical_subgraph, graph, ratio_edges),
    kp_subgraph = map_dbl(topical_subgraph, katz_powell_mutuality)
  )

# 6: plot data ------------------------------------------------------------

# The "topic-subgraph" contains all vertices that share a topic with the ego.
# For the edges in this subgraph, we can compute the ratio to the total number
# of edges, effectively asking: How big is the overlap of the topical subgraph
# and the full graph?
df %>%
  ggplot(mapping = aes(date, rat_topical_edges)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "", y = "prop. topical edges", title = "Ratio of topical edges over time")

ggsave(filename = glue("figs/{USERNAME}-1-ratio-topical-edges.png"))

# For the edges in the topic-subgraph, we can compute the difference of incoming
# and outgoing edges, relative to the ego. This provides an indication of whether
# these edges tend to be be mutual and similar in frequency (= 0), or whether
# there is an imbalance toward "receiver" (+ 1) or "sender" (- 1) behaviour.
# Normalizing by the full edge count provides an indication on whether edges
# tend to link to/from topically similar nodes. See notes for analysis of values.
df %>%
  ggplot(mapping = aes(date, topical_edges)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "", y = "topical edges", title = "Ratio of mutual topical edges over time")

ggsave(filename = glue("figs/{USERNAME}-2-ratio-mutual-topical-edges.png"))

# The Katz-Powell index of mutuality indicates whether edge choices between nodes 
# are reciprocated. Computing this measure on the subgraph indicates whether edges
# tend to be mutual if ego and alters share interests.
df %>%
  ggplot(mapping = aes(date, kp_subgraph)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "", y = "kp", title = "Katz-Powell index of the topical subgraph over time")

ggsave(filename = glue("figs/{USERNAME}-3-katz-powell.png"))

# We can measure reciprocity both as a ratio of incoming and outgoing edges, and
# by computing the Katz-Powell index of mutuality. This plot shows that they
# both arrive at similar conclusions.
df %>%
  ggplot(mapping = aes(topical_edges, kp_subgraph)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "topical edges", y = "kp", title = "Topical edges vs Katz-Powell index of subgraph")

ggsave(filename = glue("figs/{USERNAME}-5-rec-subgraph-1.png"))

# plot shows similarity of reciprocity and Katz-Powell-Index
df %>% ggplot(mapping = aes(x = date)) +
  scale_y_continuous(limits = c(-1, 1)) +
  geom_hline(yintercept = 0) +
  geom_point(mapping = aes(y = unlist(map(topical_subgraph, reciprocity, mode = "ratio")), colour="reciprocity")) +
  geom_smooth(mapping = aes(y = unlist(map(topical_subgraph, reciprocity, mode = "ratio")), colour="reciprocity")) +
  geom_point(mapping = aes(y = unlist(map(topical_subgraph, katz_powell_mutuality)), colour="Katz-Powell index")) +
  geom_smooth(mapping = aes(y = unlist(map(topical_subgraph, katz_powell_mutuality)), colour = "Katz-Powell index")) +
  labs(x = "", y = "value", colour = "measure", title = "Different reciprocity measures of the topical subgraph, over time")

ggsave(filename = glue("figs/{user}-5-rec-subgraph-1.png"))

df %>% ggplot(mapping = aes(x = date)) +
  scale_y_continuous(limits = c(-1, 1)) +
  geom_hline(yintercept = 0) +
  geom_point(mapping = aes(y = unlist(map(topical_subgraph, weighted_mutuality, node = "monocasa")), colour="weighted")) +
  geom_smooth(mapping = aes(y = unlist(map(topical_subgraph, weighted_mutuality, node = "monocasa")), colour = "weighted")) +
  geom_point(mapping = aes(y = topical_edges, colour = "topical")) +
  geom_smooth(mapping = aes(y = topical_edges, colour = "topical")) +
  labs(x = "", y = "value", colour = "measure", title = "Different reciprocity measures of the topical subgraph, over time")

ggsave(filename = glue("figs/{user}-6-rec-subgraph-2.png"))

g <- df$graph[[12]]
f <- df$topical_subgraph[[12]]

# the ego currently has a topic of -1, because we have 5 top topics for them.
# -1 can't be interpreted as a color by most (all?) palettes, so we shift all up by 1
V(g)$color <- vertex_attr(g)$topic + 1
V(f)$color <- vertex_attr(f)$topic + 1

l <- layout_with_fr(g, niter = 100000)

idx <- which(V(g)$name %in% V(f)$name)
xlim <- range(l[, 1])
ylim <- range(l[, 2])
plot(
  g,
  palette = rainbow(256),
  xlim = xlim,
  ylim = ylim,
  layout = l,
  vertex.size = 45,
  edge.width = .8,
  edge.arrow.size = .575,
  edge.arrow.width = .5,
  rescale = FALSE
)
plot(
  f,
  palette = rainbow(256),
  xlim = xlim,
  ylim = ylim,
  layout = l[idx, ],
  vertex.size = 55,
  vertex.color = NA,
  vertex.frame.color = "red",
  edge.width = 2,
  edge.arrow.mode = 0,
  add = TRUE,
  rescale = FALSE
)

V(g)$color <- ifelse(V(g)$name %in% V(f)$name, "red", "white")
l <- layout_with_fr(g, niter = 100000)
plot(
  g,
  vertex.size = 25,
  edge.width = .8,
  edge.arrow.size = .575,
  edge.arrow.width = .5,
  layout = l
)
