# Explore correlations between topic distributions and properties
# of the user's social graph.

library(tidyverse)
library(lubridate)
library(glue)
library(stringr)

source("R/network_measures/katz_powell.R")
source("R/network_measures/weighted_mutuality.R")
source("R/network_construction/filter_graph.R")
source("R/reading_data/read_snapshots.R")

# c("monocasa", formido", "cavedave", "IronWolve")
user <- c("monocasa")

load(glue("output/{user}-graph_df.rda"))
load(glue("output/{user}_corr-df.rda"))
load(glue("output/{user}-graph_analysis.rda"))

# all snapshots for one ego
ego_topics <- read_ego_topics(user)

# join graphs and topic data
df <-
  left_join(graph_data, ego_topics, by = c("year", "month", "author")) %>%
  rename(ego_topics = data)

filter_graph_wrapper <- function(graph, ego) {
  filter_graph(graph, ego$topic)
}

# Compute a measure over the reciprocal edges in the subgraph.
# This is the ratio of incoming - outgoing edges to all edges.
relative_reciprocity <- function(subg, fullg) {
  if (vcount(subg) == 0 || vcount(fullg) == 0) {
    return(NA)
  }
  
  in_subg <- length(incident(subg, user, mode = "in"))
  out_subg <- length(incident(subg, user, mode = "out"))
  
  (in_subg - out_subg) / ecount(fullg)
}

ratio_edges <- function(sg, g) {
  if (vcount(g) == 0 || vcount(sg) == 0) {
    return(NA)
  }
  
  ecount(sg) / ecount(g)
}

df <-
  df %>%
  mutate(
    date = make_date(year, month),
    topical_subgraph = map2(graph, ego_topics, filter_graph_wrapper),
    topical_edges = map2_dbl(topical_subgraph, graph, relative_reciprocity),
    rat_topical_edges = map2_dbl(topical_subgraph, graph, ratio_edges),
    kp_subgraph = map_dbl(topical_subgraph, katz_powell_mutuality)
    )

df %>%
  ggplot(mapping = aes(date, rat_topical_edges)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "", y = "prop. topical edges", title = "Ratio of topical edges over time")

ggsave(filename = glue("figs/{user}-1-ratio-topical-edges.png"))

df %>%
  ggplot(mapping = aes(date, topical_edges)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "", y = "topical edges", title = "Ratio of mutual topical edges over time")

ggsave(filename = glue("figs/{user}-2-ratio-mutual-topical-edges.png"))

df %>%
  ggplot(mapping = aes(date, kp_subgraph)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "", y = "kp", title = "Katz-Powell index of the topical subgraph over time")

ggsave(filename = glue("figs/{user}-3-katz-powell.png"))

df %>%
  ggplot(mapping = aes(topical_edges, kp_subgraph)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "topical edges", y = "kp", title = "Topical edges vs Katz-Powell index of subgraph")

ggsave(filename = glue("figs/{user}-4-topical-edges-vs-katz-powell.png"))

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
