library(igraph)
library(glue)

source("R/constants.R")

load(glue("output/{USERNAME}-graph_df.rda"))
# load(glue("output/{USERNAME}_corr-df.rda"))
# load(glue("output/{USERNAME}-graph_analysis.rda"))

g <- graph_data$graph[[25]]
f <- graph_data$topical_subgraph[[25]]

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
  layout = l[idx,],
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