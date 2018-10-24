library(igraph)
library(glue)

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


#' Ratio of incoming to outgoing edges
#'
#' In a directed graph, this computes the ratio of edges incident to \code{user}
#' against the outgoing edges.
#'
#' @param subgraph a subgraph of \code{fullgraph}
#' @param user the Ego to fetch the edges for
#'
#' @return A numeric value
#' @export
#'
#' @examples
directed_edge_ratio <- function(subgraph, user) {
  if (!is_igraph(subgraph)) {
    stop("The provided subgraph is not a graph object")
  }
  
  if (vcount(subgraph) == 0) {
    return(NA)
  }
  
  if (!user %in% V(subgraph)$name) {
    stop(glue("{user} is not a node in the subgraph"))
  }
  
  in_subg <- length(incident(subgraph, user, mode = "in"))
  out_subg <- length(incident(subgraph, user, mode = "out"))
  
  in_subg / out_subg
}
