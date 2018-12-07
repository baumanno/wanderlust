library(igraph)

#' Compute the Katz-Powell-Index of mutuality
#'
#' See [Wassermann p.514]; the index allows comparisons of mutuality across
#' varying network sizes. This is a global measure for the entire network.
#' However, since we consider only ego->alter and alter->ego, but not
#' alter->alter ties, this can be viewed as a measure describing the structure
#' of ties for a single ego
#'
#' @param graph An igraph object.
#'
#' @return The Katz-Powell index value
#' @export
#'
katz_powell_mutuality <- function(graph) {
  vapply(
    X = graph,
    FUN = function(graph) {
      
      graph <- simplify(graph)
      
      if (ecount(graph) == 0) {
        return(NA)
      }
      
      L <- sum(degree(graph, mode = "out"))
      L2 <- sum(degree(graph, mode = "out") ^ 2)
      g <- vcount(graph)
      M <- dyad_census(graph)$mut
      
      (2 * ((g - 1) ^ 2) * M - L^2 + L2) / (L * ((g - 1) ^ 2) - L ^ 2 + L2)
    },
    FUN.VALUE = double(1)
  )
}
