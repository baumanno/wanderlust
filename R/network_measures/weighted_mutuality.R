library(igraph)

#' Compute weighted mutuality (better word?)
#'
#' For a given node V, compute the normalized difference of incoming
#' and outgoing edges.
#' This measure varies in [-1;1], with 0 denoting the balanced case where
#' |in| = |out|
#' A source node will exhibit a value close to -1, a sink node close to 1.
#'
#' @param graph An igraph graph object.
#' @param node A string. The vertex-name to compute the index for.
#'
#' @return A numeric value
#' @export
#'
#' @examples
weighted_mutuality <- function(graph, node) {
  if (vcount(graph) == 0)
    return(NA)
  
  # outdegree d+
  out <- length(incident(graph, node, mode = "out"))
  
  # indegree d-
  inc <- length(incident(graph, node, mode = "in"))
  
  total <- inc + out
  
  # no mutuality for isolated nodes
  if (total == 0)
    return(NA)
  
  (inc - out) / total
}


#' Relative reciprocity of a set of edges
#' 
#' Computes the difference of incident and outgoing edges of a subgraph 
#' relative to the number of edges in the original full graph.
#'
#' @param subgraph a subgraph of \code{fullgraph}
#' @param fullgraph an igraph graph
#'
#' @return
#' @export
#'
#' @examples
relative_reciprocity <- function(subgraph, fullgraph, user) {
  if (vcount(subgraph) == 0 || vcount(fullgraph) == 0) {
    return(NA)
  }
  
  in_subg <- length(incident(subgraph, user, mode = "in"))
  out_subg <- length(incident(subgraph, user, mode = "out"))
  
  (in_subg - out_subg) / ecount(fullgraph)
}
