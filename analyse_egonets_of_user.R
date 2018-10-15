library(tidyverse)
library(glue)
library(gridExtra)
library(igraph)
library(lubridate)

source("R/reading_data/read_snapshots.R")
source("R/network_construction/build_from_edgelist.R")
source("R/network_measures/katz_powell.R")
source("R/network_measures/weighted_mutuality.R")
source("R/network_measures/krackhardt.R")

user <- c("monocasa")#, "formido", "cavedave", "IronWolve")

edgelists <- read_edgelist(user)

topic_metadata_df <- read_alters_topics(user)

topic_metadata_df <- read_monthly_snapshot_data(user, path = "data/{user}/alters-topics-{year}-{month}.csv")

topic_metadata_df <- lapply(topic_metadata_df, function(x) {
  x %>% 
    select(-subreddit, -maxcount, -year, -month) %>% 
    add_row(author = user, topic = -1)
})

# ... and construct a network from it
list_of_graphs <-
  build_egonet_from_edgelist(edgelists, topic_metadata_df)

save(list_of_graphs, file = "output/list_of_graphs.rda")

graph_store <-
  transmute(
    month,
    year,
    author = user,
    vertices = sapply(list_of_graphs, vcount),
    katz_powell = sapply(list_of_graphs, katz_powell_mutuality),
    weighted = mapply(weighted_mutuality, list_of_graphs, author)
  )

save(graph_store, file = "output/graph_store.rda")

a <-
  ggplot(graph_store, mapping = aes(x = make_date(year, month), y = vertices)) +
  geom_point(alpha = 1 / 10) +
  geom_smooth()
b <-
  ggplot(graph_store, mapping = aes(x = make_date(year, month), y = katz_powell)) +
  geom_point(aes(colour = katz_powell), alpha = 1 / 10) +
  geom_smooth()
c <-
  ggplot(graph_store, mapping = aes(x = make_date(year, month), y = weighted)) +
  geom_point(aes(colour = weighted), alpha = 1 / 10) +
  geom_smooth() +
  scale_y_continuous(limits = c(-1, 1))
g_a <- grid.arrange(a, b, c, nrow = 3)

d <-
  ggplot(graph_store, mapping = aes(x = vertices, y = katz_powell)) +
  geom_point(colour = "blue", alpha = 1 / 10) +
  geom_smooth()
e <-
  ggplot(graph_store, mapping = aes(x = vertices, y = weighted)) +
  geom_point(colour = "blue", alpha = 1 / 10) +
  geom_smooth()

g_b <- grid.arrange(d, e, nrow = 2)

p <- grid.arrange(g_a, g_b, ncol = 2)
ggsave("figs/foo.png", p)
