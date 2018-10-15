


# 0: Libraries ------------------------------------------------------------

library(tidyverse)
library(glue)
library(gridExtra)
library(igraph)
library(lubridate)


# 1: External functions ---------------------------------------------------

source("R/reading_data/read_snapshots.R")
source("R/network_construction/build_from_edgelist.R")
source("R/network_measures/katz_powell.R")
source("R/network_measures/weighted_mutuality.R")
source("R/network_measures/krackhardt.R")


# 2: global constants -----------------------------------------------------

#user <- c("monocasa", "formido", "cavedave", "IronWolve")
user <- "IronWolve"

# 3: Read data from snapshots ---------------------------------------------

edgelists <- read_edgelist(user)
topics_of_alters <- read_alters_topics(user)

# the ego is missing from the alters snapshots, so we add it manually with a
# special topic of -1

fix_missing_ego <- function(df) {
  if (nrow(df) == 0) {
    return(df)
  }
  add_row(df, author = user, topic = -1)
}

topics_of_alters <-
  topics_of_alters %>%
  mutate(data = map(data, fix_missing_ego))


# 4: aggregate data into single dataframe ---------------------------------

# store edgelists and topic metadata in a frame
df <-
  left_join(edgelists, topics_of_alters, by = c("month", "year", "author")) %>%
  select(year,
         month,
         author,
         edgelist = data.x,
         alters_topics = data.y)

# remove the now unused objects
rm(edgelists, topics_of_alters)

# 5: analysis of egonetwork -----------------------------------------------

df <- df %>%
  mutate(graph = map2(edgelist, alters_topics, egonet_from_edgelist))

graph_analysis <- df %>%
  mutate(
    vertices = map_int(graph, vcount),
    katz_powell = map_dbl(graph, katz_powell_mutuality),
    weighted = map_dbl(graph, weighted_mutuality, node = author)
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
