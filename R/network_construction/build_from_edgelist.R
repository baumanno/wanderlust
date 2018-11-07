library(igraph)
library(tidyverse)

#' Build an ego-network from an edgelist
#'
#' Takes a list of ego-alter edges and constructs a graph from this. For empty
#' edgelists, empty graphs are returned.
#'
#' @param edgelist A two-column data.frame with source-sink pairs.
#'
#' @return An igraph graph object.
#' @export
egonet_from_edgelist <- function(edgelist, vertices = NULL) {

  if (nrow(edgelist) == 0) {
    return(make_empty_graph())
  }

  g <- graph_from_data_frame(edgelist, vertices = vertices, directed = TRUE)

  assertthat::assert_that(is.igraph(g))

  g
}
