library(igraph)

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