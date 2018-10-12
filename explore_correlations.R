# Explore correlations between topic distributions and properties
# of the user's social graph.

library(tidyverse)
library(lubridate)
library(glue)
library(stringr)

source("05_network_measures/katz_powell.R")
source("05_network_measures/weighted_mutuality.R")

load("graph_store.rda")
load("aligned_data.rda")
load("list_of_graphs.rda")

#' Read a data file containing snapshot data
#' 
#' Given a username and path, this function constructs
#' the full path to the snapshot-file. This file is read 
#' and the resulting tibble returned in a list over all dates 
#' in the range.
#' The path is interpreted with `glue_data` and 
#' thus can contain parameters enclosed in `{}`.
#' The optional column specification allows fine-tuning of the
#' data-types.
#'
#' @param user 
#' @param path 
#' @param cols 
#'
#' @return
#' @export
#'
#' @examples
read_monthly_snapshot_data <- function(user, path, cols = NULL) {
  years <- 2007:2018
  months <- 1:12
  
  df <- cross_df(list(
    month = months,
    year = years,
    user = user
  ))
  
  # we only want to look at data from 11-2007 to 02-2018
  df <- df[-which(df$month < 11 & df$year == 2007),]
  df <- df[-which(df$month > 2 & df$year == 2018),]
  
  filelist <- glue_data(df, path)
  
  lapply(filelist, function(fname) {
    frame <- read_csv(fname, col_types = cols)

    # For some data, the year and month can be missing, 
    # so we attempt to extract them from the filename.
    if (!("year" %in% colnames(frame)) && !("month" %in% colnames(frame))) {
      
      d <- fname %>% 
        str_extract(. , "\\d{4}-\\d{1,2}") %>% 
        str_split(. , "-")
      
      frame$year <- as.integer(rep_len(d[[1]][1], nrow(frame)))
      frame$month <- as.integer(rep_len(d[[1]][2], nrow(frame)))
    }
    
    frame
  })
}

#' Create a subgraph containing the specified topic nodes only.
#'
#' Filters the nodes in the given graph to only return
#' those matching the topic number, returning the subgraph containing these
#' nodes.
#' 
#' @param graph 
#' @param topic_number 
#'
#' @return An igraph graph
filter_graph <- function (g, topic_numbers) {
  
    if(length(V(g)) == 0) {
      return(make_empty_graph())
    }
    
    # We must remove nodes that don't have a topic assigned,
    # as these will cause problems when iterating over the graph.
    g <- g - V(g)[is.na(topic)]
    
    # the -1 topic is reserved for the ego, as it doesn't have
    # a single top topic, but a list of N top topics
    vs <- V(g)[topic %in% topic_numbers | topic == -1]
    
    if (length(vs) == 0) {
      return(NA)
    }
    
    induced_subgraph(g, vs)
}

# c("monocasa", formido", "cavedave", "IronWolve")
user <- c("monocasa")

# all snapshots for one ego
ego_topics <-
  read_monthly_snapshot_data(
    user = user,
    path = "_cases/{user}/ego-topics-{year}-{month}.csv",
    cols = cols(
      year = col_integer(),
      month = col_integer(),
      author = col_character(),
      subreddit = col_character(),
      topic = col_integer(),
      count = col_integer()
    )
  )

df <- graph_store

df$ego <- ego_topics
df$graph <- list_of_graphs

rm(graph_store)
rm(list_of_graphs)

filter_graph_wrapper <- function(graph, ego) {
  filter_graph(graph, ego$topic)
}

# Store the topical subgraph in the tibble
df <- df %>% mutate(
  topical_subgraph = map2(graph, ego, filter_graph_wrapper)
)

# Compute a measure over the reciprocal edges in the subgraph.
# This is the ratio of incoming - outgoing edges to all edges.
df <- df %>% mutate(
  topical_edges = map2_dbl(df$topical_subgraph, df$graph, function(subg, fullg) {
    
    if(vcount(subg) == 0 || vcount(fullg) == 0) {
      return(NA)
    }
    
    in_subg <- length(incident(subg, user, mode = "in"))
    out_subg <- length(incident(subg, user, mode = "out"))
    
    (in_subg - out_subg) / ecount(fullg)
  })
)

df <- df %>% mutate(
  date = make_date(year, month),
  kp_subgraph = map_dbl(df$topical_subgraph, katz_powell_mutuality)
)

df <- df %>% mutate(
  rat_topical_edges = map2_dbl(topical_subgraph, graph,function (sg, g) {
    if(vcount(g) == 0 || vcount(sg) == 0) {
      return(NA)
    }
    
    ecount(sg) / ecount(g)
  })
)

df %>% 
  ggplot(mapping = aes(date, rat_topical_edges)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "", y = "topical edges",title = "Ratio of topical edges over time")

df %>% 
  ggplot(mapping = aes(date, topical_edges)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "", y = "topical edges",title = "Ratio of mutual topical edges over time")

df %>% 
  ggplot(mapping = aes(date, kp_subgraph)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "", y = "kp", title = "Katz-Powell index of the topical subgraph over time")

df %>%
  ggplot(mapping = aes(topical_edges, kp_subgraph)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(limits = c(-1, 1)) + 
  labs(x = "topical edges", y = "kp", title = "Topical edges vs Katz-Powell index of subgraph")


# plot shows similarity of reciprocity and Katz-Powell-Index
df %>% ggplot(mapping = aes(x = date)) +
  scale_y_continuous(limits = c(-1, 1)) +
  geom_hline(yintercept = 0) +
  geom_point(mapping = aes(y = unlist(map(topical_subgraph, reciprocity, mode = "ratio")), colour="reciprocity")) +
  geom_smooth(mapping = aes(y = unlist(map(topical_subgraph, reciprocity, mode = "ratio")), colour="reciprocity")) +
  geom_point(mapping = aes(y = unlist(map(topical_subgraph, katz_powell_mutuality)), colour="Katz-Powell index")) +
  geom_smooth(mapping = aes(y = unlist(map(topical_subgraph, katz_powell_mutuality)), colour = "Katz-Powell index")) +
  labs(x = "", y = "value", colour = "measure", title = "Different reciprocity measures of the topical subgraph, over time")

df %>% ggplot(mapping = aes(x = date)) +
  scale_y_continuous(limits = c(-1, 1)) +
  geom_hline(yintercept = 0) +
  geom_point(mapping = aes(y = unlist(map(topical_subgraph, weighted_mutuality, node = "monocasa")), colour="weighted")) +
  geom_smooth(mapping = aes(y = unlist(map(topical_subgraph, weighted_mutuality, node = "monocasa")), colour = "weighted")) +
  geom_point(mapping = aes(y = topical_edges, colour = "topical")) +
  geom_smooth(mapping = aes(y = topical_edges, colour = "topical")) +
  labs(x = "", y = "value", colour = "measure", title = "Different reciprocity measures of the topical subgraph, over time")



g <- df$graph[[12]]
f <- df$topical_subgraph[[12]]

# the ego currently has a topic of -1, because we have 5 top topics for them.
# -1 can't be interpreted as a color by most (all?) palettes, so we shift all up by 1
V(g)$color <- vertex_attr(g)$topic + 1
V(f)$color <- vertex_attr(f)$topic + 1

l <- layout_with_fr(g,niter=100000)

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
  edge.arrow.size=.575,
  edge.arrow.width=.5,
  rescale = FALSE
)
plot(
  f,
  palette = rainbow(256),
  xlim = xlim,
  ylim = ylim,
  layout = l[idx, ],
  vertex.size = 55,
  vertex.color=NA,
  vertex.frame.color="red",
  edge.width = 2,
  edge.arrow.mode=0,
  add=TRUE,
  rescale = FALSE
)

V(g)$color <- ifelse(V(g)$name %in% V(f)$name, "red", "white")
l <- layout_with_fr(g,niter=100000)
plot(
  g,
  vertex.size = 25,
  edge.width = .8,
  edge.arrow.size=.575,
  edge.arrow.width=.5,
  layout=l
)
