library(igraph)

#' Compute Krackhardt and Stern's E-I index
#' 
#' The index is a measure for homophily/heterophily. For a given node, it
#' measures how many ties are to the same group (\emph{I}, internal) and how many are
#' external (\emph{E}).
#'
#' A value of -1 denotes homophily, a value of 1 heterophily.
#'
#' @param graph An igraph graph object.
#' @param group_by A string. This value denotes the "internal" ties.
#' @param ego A string. The vertex-name to consider as ego.
#'
#' @return The Krackhardt-Stern E-I index
#' @export
#'
krackhardt_ei <- function(graph, group_by, ego) {
  reference_group <-
    get.vertex.attribute(graph, name = "topic", index = ego)
  
  external <- V(graph)[eval(parse(text = group_by)) != reference_group]
  E <- length(external)
  
  internal <- V(graph)[eval(parse(text = group_by)) == reference_group]
  # don't count in the ego
  I <- length(internal) - 1
  
  (E - I) / (E + I)
}