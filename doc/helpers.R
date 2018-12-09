library(tidyverse)
library(igraph)

fmt <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",")
}

makeSummary <- function(df, column) {
  df %>%
    summarise(
      N = n(),
      "arithm. Mittel" = mean(column),
      SD = sd(column),
      Min = min(column),
      Q1 = quantile(column, .25),
      Median = median(column),
      Q3 = quantile(column, .75),
      Max = max(column)
    )
}

vvcount <- Vectorize(vcount, vectorize.args = list("graph"))
vecount <- Vectorize(ecount, vectorize.args = list("graph"))

degree_ratio <- function(graph) {
  if (vcount(graph) == 0) {
    return(0.0)
  }
  
  modes <- c("in", "out")
  degs <- map(modes, function(m) {
    degree(graph, v = user, mode = m)
  })
  do.call(`/`, degs)
}

filter_graph_wrapper <- function(graph, ...) {

  graph <- set_vertex_attr(graph, "topic", user, -1)
  
  filter_graph(graph, ...)
}

degree_ratio_wrapper <- function(graph) {
  if (length(V(graph)) == 0) {
    return(0)
  }
  
  if (!user %in% get.vertex.attribute(graph, "name")) {
    # due to filtering, the ego might be dropped from the graph,
    # so we must patch it back in
    graph <- graph + vertex(user)
  }
  
  degree_ratio(graph)
}

vdegree <- function(graph, v, mode) {
  vapply(
    X = graph,
    FUN = function(graph, v, mode) {
      if (!user %in% get.vertex.attribute(graph, "name")) {
        # due to filtering, the ego might be dropped from the graph,
        # so we must patch it back in
        graph <- graph + vertex(user)
      }
      
      degree(graph = graph,
             v = v,
             mode = mode)
    },
    FUN.VALUE = double(1),
    v,
    mode
  )
}

pronounce <- function(number) {
  words <-
    c(
      "eins",
      "zwei",
      "drei",
      "vier",
      "fünf",
      "sechs",
      "sieben",
      "acht",
      "neun",
      "zehn",
      "elf",
      "zwölf"
    )
  
  if (number <= 12) {
    return(words[number])
  }
  
  number
}